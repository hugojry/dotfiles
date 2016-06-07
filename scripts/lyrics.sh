lyrics_api="http://makeitpersonal.co/lyrics/"

curl -s --get "$lyrics_api" \
    --data-urlencode "artist=$1" \
    --data-urlencode "title=$2"
