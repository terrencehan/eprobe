eprobe - A profiling toolkit for Erlang.
=========================

Status
======
This toolkit is under early development. So, APIs are still flux and may be changed
quickly without notice.

Components
=========
##eprobe_trace

####Features
* Visualization by Flame Graph
* Combine parallel processes within single picture

Inspired by [eflame](https://github.com/proger/eflame), eprobe_trace provides a
method to trace your application in time sequence, and visualise call stacks using Flame
Graph.

Compared to original implementation, `script/flamegraph.pl` introduced a new concept
`layer` to show the relationship between parallel processes, even those in separate nodes.
So, all processes could be combined in one flame graph, ordered by time they took.
<p align="center">
  <img src="https://raw.github.com/terrencehan/eprobe/master/sample/flame.png?raw=true" alt=""/>
</p>

####Example
See [https://github.com/terrencehan/eprobe/blob/master/sample/trace.erl](https://github.com/terrencehan/eprobe/blob/master/sample/trace.erl)

COPYRIGHT AND LICENCE
------------------------

Copyright (C) 2014 by terrencehan (isterrence@gmail.com)

This library is free software; you can redistribute it and/or modify
it under the MIT License.
