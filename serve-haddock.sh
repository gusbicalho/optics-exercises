#! /bin/bash

export PORT="3333"
export INDEX="$(stack haddock 2>&1 | grep all/index.html)"

echo "URL: http://localhost:$PORT$INDEX"
echo "If URL does not work, check if portforwarding is active."
which static > /dev/null && {
  static -p "$PORT" / &
} || {
  echo '"static" not found in path. Try running: npm install -g node-static'
}

stack haddock --file-watch
