#!/usr/bin/env python3
from google.cloud import storage
import gzip
import html
import json
import math
import re
import requests
import sys
import time

###############################################################################
##                                                                           ##
## Fetch posts from reddit and cache in a GCS bucket so f6oclock doesn't     ##
## have to suffer the reddit APIs high latency on initial page-load.         ##
##                                                                           ##
## The refresh rate is a function of how busy r/politics is. This saves      ##
## by not unnecesarily updating the GCS cache object.                        ##
##                                                                           ##
###############################################################################

# We refresh somewhere between every REFRESH_MIN seconds and REFRESH_MAX
# seconds depending on how much is happening between refreshes. REFRESH_BASE
# controls how fast it (exponentially) moves between the min and max.
REFRESH_MIN = 30 # 30 seconds
REFRESH_MAX = 3600 # 1 hour
REFRESH_BASE = 2 # base for exponential increase/decay of refresh rate
REFRESH_UP = 1.5 # speed at which exponent moves when uping the refresh rate
REFRESH_DOWN = 1
STREAK_MAX = math.ceil(math.log(REFRESH_MAX - REFRESH_MIN + 1, REFRESH_BASE))

# These numbers control how much weight is given to changes in score when
# computing the diff for two scoreboards. These values are chosen to match the
# ones in the front-end that control colouring. The goal is to approximate how
# much the color changed for each post between refreshes.
VOTES_MIN = 20
VOTES_MAX = 700
VOTES_STEP = 150 # each one of these counts for one "diff" point

# Clamp the pre-normalized delta scores between these two numbers
DELTA_MIN = 2
DELTA_MAX = 100
# The cutoff to consider a normalized delta significant
DELTA_CUTOFF = 0.5

# Get data from reddit.com
def fetch_data():
    headers = {
        'user-agent': 'f6oclock/1.0.0'
    }

    res = requests.get(
        url = 'https://www.reddit.com/r/politics/rising.json',
        headers = headers
    )

    return sorted(
        res.json()['data']['children'],
        key = lambda post: post['data']['ups'],
        reverse = True
    )

# Extracts the ids and vote numbers for the posts. This minus the links and
# titles is what is shown on f6oclock
def get_scoreboard(posts):
    entries = [get_scoreboard_entry(post) for post in posts]

    return {
        "ids": [post[0] for post in entries],
        "votes": [post[1] for post in entries]
    }

def get_scoreboard_entry(post):
    data = post['data']
    return (data['id'], data['ups'])

# Exponentially move between REFRESH_MIN/REFRESH_MAX depending on recent
# history of refresh productivity
def get_next_refresh(streak):
    refresh = REFRESH_MIN + REFRESH_BASE**abs(streak) - 1
    return clamp(REFRESH_MIN, REFRESH_MAX, refresh)

# Compute a delta between two scoreboards. This is intended to be a qualitative
# measure of the value of this cache refresh to the f6oclock user.
def compute_delta(prev, cur):
    delta = 0

    for idx, cur_id in enumerate(cur["ids"]):
        cur_votes = cur["votes"][idx]
        cur_votes_norm = normalize_votes(cur_votes)

        try:
            prev_idx = prev["ids"].index(cur_id) # O(n^2)
        except ValueError:
            prev_idx = None

        if prev_idx == None:
            dvotes_norm = cur_votes_norm
            didx = max(0, len(prev) - idx)
        else:
            prev_votes = prev["votes"][prev_idx]
            prev_votes_norm = normalize_votes(prev_votes)
            dvotes_norm = abs(prev_votes_norm - cur_votes_norm)
            didx = abs(idx - prev_idx)

        delta += didx
        delta += dvotes_norm

    print('delta = ' + str(delta))
    delta_norm = normalize(DELTA_MIN, DELTA_MAX, delta)
    print('delta_norm = ' + str(delta_norm))
    delta_smooth = smoothstep(delta_norm)
    print('delta_smooth = ' + str(delta_smooth))

    return delta_smooth

# Maps [0, inf] to an integer in [0, floor((VOTES_MAX-VOTES_MIN)/VOTES_STEP)]
def normalize_votes(votes):
    return int(normalize(VOTES_MIN, VOTES_MAX, float(votes))*VOTES_STEP)

# Map [-inf, inf] to [xmin, xmax] with a clamp and then to [0, 1] linearly
def normalize(xmin, xmax, x):
    clamped = clamp(xmin, xmax, x)
    return float(clamped - xmin)/(xmax - xmin)

# Map [-inf, inf] to [xmin, xmax] with thresholding
def clamp(xmin, xmax, x):
    return max(min(x, xmax), xmin)

# Hermite polynomial/s-curve. Maps from [0, 1] to [0, 1] smoothly
def smoothstep(x):
    if x <= 0:
        return 0
    if x >= 1:
        return 1

    return (3 -2*x)*x*x

def sign(x):
    if x < 0:
        return -1
    return 1

# Store the raw reddit response into GCS
def set_cache(bucket, res):
    blob = storage.Blob('index.html', bucket)

    # TODO between REFRESH_MIN and something else cache lifetime?
    blob.cache_control = 'public, max-age=' + str(REFRESH_MIN)

    blob.content_encoding = 'gzip'

    data = bytes(res.text, 'utf-8')
    compressed = gzip.compress(data)

    blob.upload_from_string(
        data = compressed,
        content_type = 'application/json',
    )

    print('cached')

def render(posts):
    with open('../index.html', 'r') as f:
        template = f.read()

    # This knows about the Elm type Post
    posts = [{
        'id': post['data']['id'],
        'upvotes': post['data']['ups'],
        'createdUtc': post['data']['created_utc'],
        'domain': post['data']['domain'],
        'url': post['data']['url'],

        # TODO: if elm did unescape @ render wouldn't need to do it here.
        'title': html.unescape(post['data']['title']),

        # TODO: elm should do this at render time
        'permalink': 'https://www.reddit.com' + post['data']['permalink'],

    } for post in posts]

    data = json.dumps(posts, separators=(',', ':'))

    return template.replace(
        'const cache = { posts: [] };',
        'const cache={posts:'+ data + '};'
    )

###############################################################################
###############################################################################
###############################################################################

client = storage.Client(project='f6oclock')
bucket = client.get_bucket('www.f6oclock.com')

# We don't load what's actually in the cache on boot so we will always do one
# store.
prev = {
    "votes": [],
    "ids": [],
}

# start off with a sleep so that if this program restarta a bunchs it doesnt
# refresh faster than the minimum
next_refresh = REFRESH_MIN

# the first iteration will have a big enough delta so streak will go to 0.
streak = -1

while True:
    time.sleep(next_refresh)

    posts = fetch_data()

    cur = get_scoreboard(posts)

    delta = compute_delta(prev, cur)

    # If the difference was negligble we pretend that we don't store the result
    # and we also don't update prev (that way we are always diffing against
    # what was cached (unless this is on boot)
    if delta > DELTA_CUTOFF:
        streak -= REFRESH_UP
        res = render(posts)
        set_cache(bucket, res)
        prev = cur
    else:
        streak += REFRESH_DOWN

    streak = clamp(0, STREAK_MAX, streak)

    next_refresh = get_next_refresh(streak)
    print('streak = ' + str(streak))
    print('next_refresh = ' + str(next_refresh))
    print('')    
