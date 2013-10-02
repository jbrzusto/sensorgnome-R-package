#!/bin/bash
git archive --format=tar --remote=john@discovery:/home/git/filter_tags HEAD | (cd src/filter_tags; tar xf -)
git archive --format=tar --remote=john@discovery:/home/git/find_tags HEAD | (cd src/find_tags; tar xf -)


