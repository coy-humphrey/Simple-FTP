#!/bin/sh
pandoc report.markdown -o report.pdf -V geometry:margin=1in -s --highlight-style tango
pandoc report.markdown -o report.html -s --highlight-style tango