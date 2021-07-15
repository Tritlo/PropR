#!/bin/bash
echo "Starting Repair-Docker - Building the command"

# As the command gets maybe complex, 
# it is stitched together with any flag set in docker. 

# Repairtarget must be set, otherwise it makes little sense
commandCollector="cabal run endemic -- $REPAIR_TARGET"

if [ -z ${LOG_LEVEL+x} ]; 
then echo "Log-Level is not set"; 
else commandCollector="$commandCollector --log-level=$LOG_LEVEL "; 
fi

if [ -z ${LOG_FILE+x} ]; 
then echo "Log-File is not set"; 
else commandCollector="$commandCollector --log-file=$LOG_FILE "; 
fi

if [ -z ${CONFIG_FILE+x} ]; 
then echo "Configuration-File is not set"; 
else commandCollector="$commandCollector --config=$CONFIG_FILE "; 
fi

if [ -z ${CONFIG_OVERRIDE+x} ]; 
then echo "No Configuration Override given"; 
else commandCollector="$commandCollector --override=$CONFIG_OVERRIDE "; 
fi

if [ -z ${NO_TIMESTAMP+x} ]; 
then echo "TimeStamps will be printed"; 
else commandCollector="$commandCollector --no-log-timestamp "; 
fi

if [ -z ${SEED+x} ]; 
then echo ""; 
else commandCollector="$commandCollector --seed=$SEED "; 
fi

echo "final cmd is ${commandCollector}"

/bin/bash -c "$commandCollector"

# To keep the container open for inspection
# echo "Program finished - Keeping Container open for inspection"
# tail -f /dev/null