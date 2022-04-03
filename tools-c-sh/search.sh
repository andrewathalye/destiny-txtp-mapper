#!/bin/sh
echo "$0: Find txtps and sort them by length" 1>&2
for i in txtp/*.txtp
do
	echo $(~/bin/vgmstream-cli -m "$i" | tail -n1 | cut -d' ' -f3) $(echo "$i" | cut -b6-19)
done
