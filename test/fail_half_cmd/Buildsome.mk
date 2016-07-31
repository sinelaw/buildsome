output.txt:
	echo hi > output.txt
	false
	echo bye >> output.txt

.PHONY: default
default: output.txt
