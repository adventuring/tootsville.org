(in-package :Tootsville)

(defun choose-next-cloud (server-kind)
  "This uses the  values in *SCALING$$$* (read aloud  as: “scaling price
ratios”) to determine  which cloud provider will be chosen  for the next
host's deployment.

The intention is that the numbers  given in *SCALING$$$* are meant to be
the  relative  desireability  of  deploying  to  each  hosting  service.
Particularly,   the  hourly   cost   of  operating   a   host  on   that
cloud, inverted. (A host that costs 35¢ / hour would instead be 1/0.35.

The assignment algorithm goes as follows:

@itemize
@item 

For the given server-type, there should be at least 2 clouds represented
if  there are  at least  2  hosts running.  This means,  if all  running
servers are on the same cloud, the next one should not be on that same cloud.

@item

If automatic  monitoring of a cloud  is able to determine  that there is
some error  or warning condition  exists, then  it will be  removed from
consideration for launching new instances.

@item

When allocating hosts, once any clouds are removed as required, then the
remaining clouds  are compared against  the relative numbers  of servers
that each currently  has running (of this type). The  next cloud will be
chosen in such a way that it will most closely match the relative scalar
of *SCALING$$$*.

@end itemize

"
  )
