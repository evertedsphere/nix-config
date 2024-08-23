#!/usr/bin/env bash
set -euo pipefail

echo "Waiting for Mullvad to come up…"
while ! mullvad status >/dev/null; do sleep 1; done

echo "Configuring Mullvad…"
mullvad auto-connect set on
mullvad lockdown-mode set on
mullvad relay set tunnel-protocol wireguard
mullvad relay set location fr
