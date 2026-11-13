#!/bin/sh
# tests/mcclim/run.sh — McCLIM on Clysm: the test suite, then an application
# on a virtual X server (Xvfb), checked by a screenshot and a typed command.
# Needs Quicklisp with McCLIM, Xvfb, xdotool and ImageMagick (import, convert).
cd "$(dirname "$0")/../.." || exit 1
out=build/mcclim-test; mkdir -p $out
for tool in Xvfb xdotool import convert; do
  command -v $tool >/dev/null || { echo "mcclim: $tool not found, skipped"; exit 0; }
done
disp=:${MCCLIM_TEST_DISPLAY:-87}
Xvfb $disp -screen 0 800x600x24 -nolisten tcp > $out/xvfb.log 2>&1 &
xvfb=$!
trap 'kill $xvfb 2>/dev/null; kill $app 2>/dev/null' EXIT
sleep 1
export DISPLAY=$disp
status=0

bin/clysm script tests/mcclim/test-suite.lisp > $out/suite.log 2>&1
grep -E '^(checks|failed tests):' $out/suite.log
grep -q 'mcclim tests: ok' $out/suite.log || { echo "mcclim: test suite failed (see $out/suite.log)"; status=1; }

bin/clysm script tests/mcclim/hello.lisp > $out/hello.log 2>&1 &
app=$!
i=0; until grep -q ready $out/hello.log || [ $i -ge 600 ] || ! kill -0 $app 2>/dev/null; do sleep 1; i=$((i+1)); done
sleep 2
import -window root $out/hello.png
# the circle is red, the rectangle light blue
circle=$(convert $out/hello.png -format '%[hex:p{300,80}]' info:)
rect=$(convert $out/hello.png -format '%[hex:p{100,70}]' info:)
echo "pixels: circle=$circle rectangle=$rect"
[ "$circle" = FF0000 ] && [ "$rect" = ADD8E6 ] || { echo "mcclim: unexpected drawing (see $out/hello.png)"; status=1; }
xdotool mousemove 200 350 click 1; sleep 1
xdotool type --delay 100 Quit; sleep 1; xdotool key Return
i=0; until grep -q 'frame exited' $out/hello.log || [ $i -ge 60 ] || ! kill -0 $app 2>/dev/null; do sleep 1; i=$((i+1)); done
grep -q 'frame exited' $out/hello.log || { echo "mcclim: the Quit command did not exit the frame (see $out/hello.log)"; status=1; }
[ $status -eq 0 ] && echo "mcclim: ok"
exit $status
