#!/bin/sh
# FIXME - check if already done?
echo 145 > /sys/class/gpio/export
echo "high" > /sys/class/gpio/gpio145/direction
