# Control Theory in Haskell

This module is the beginning of a set of data structures, algorithms,
transfomers and adapters that implement concepts from control theory to modify
the behavior of state machines and queues along the lines of
proportional/integral/derivative controllers.

Some concepts:

 - Feedback loops
 - Rate-limiting
 - Exponential back-off
 - Introducing jitter
 - Multi-point control systems
 - Lag sensitivity (i.e., data freshness)
 - Queue exhaustion
 - Event vs. level triggering
 - Flow control (using feedback to determine fair flow rates)
 - Multi-variate controllers
 - Hysteresis (history dependent behavior)

Example applications:

 - Only spooling the same print job N times within X minutes

 - Have 'make -j' be adaptive based on available memory and load average. This
   could be implemented by simply stopping (SIGSTP) and resuming child
   processes based on system feedback.

Data structures and algorithms:

 - Queue adapters (also called moderators, governors)
 - Age-based membership
 - Retrying adapter with above behaviors (also called a mediator)
 - Window-based triggers
