#!/bin/bash
echo "Starting Astor - Building the command"

# As the command gets maybe complex, 
# it is stitched together with any flag set in docker. 

# Repairtarget must be set, otherwise it makes little sense? 
commandCollector="cabal run endemic -- $REPAIR_TARGET"

if [ -z ${LOG_LEVEL+x} ]; 
then echo "Log-Level is not set"; 
else commandCollector="$commandCollector --log=$LOG_LEVEL "; 
fi

echo "final cmd is ${commandCollector}"

/bin/bash -c "$commandCollector"

# To keep the container open for inspection
# echo "Keeping Container open for inspection"
# tail -f /dev/null