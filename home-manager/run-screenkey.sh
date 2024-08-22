set -euo pipefail

# Find the right line in the xrandr output and parse the geometry specification
read -r screen_w screen_h screen_x screen_y < <(xrandr |
    sed 's/primary //' |
    grep "$(i3-msg -t get_workspaces | jq -cr '.[] | select(.focused).output')" |
    cut -d' ' -f3 |
    sed 's/[^0-9]/ /g')

window_margin="$((screen_w / 15))"
window_height="$((screen_h / 10))"

# Place the window at the bottom of the screen, 'window_margin' away from the edges
# as wide as possible, but with 'window_height' height
geometry_w="$((screen_w - 2 * window_margin))"
geometry_h="$((window_height))"
geometry_x="$((screen_x + window_margin))"
geometry_y="$((screen_y + screen_h - window_height - window_margin))"
geometry="${geometry_w}x${geometry_h}+${geometry_x}+${geometry_y}"

screenkey \
    --vis-shift \
    --key-mode composed \
    --mods-mode emacs \
    --opacity 0.8 \
    --compr-cnt 5 \
    --timeout 2 \
    --font-size small \
    --position fixed \
    --geometry "$geometry"
