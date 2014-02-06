---
layout: post
title:  "Do messages get lost when erlang modules are upgraded?"
categories: thesis erlang
---

One of the problems that I am currently dealing with in my thesis is
the following:

How to make sure that the messages arriving
during an upgrade don't get dropped?

First , we have to make sure what happens in erlang. According to the
erlang manual, erlang keeps 2 versions of the module in memory i.e old
and current.  When a module is upgraded,

* the old version is discarded
* processes running the old version crash(or maybe are killed)
* the current version is marked as old
* the new version becomes current

Fully qualified function calls of the from ?MODULE:foo() always refer
to the current version.  Non qualifited function calls such as foo()
refer to the version in which they were invoked.

So, code reloading boils down to simple using a fully qualified function call!

Coming back to the original question, what happens to the messages in
transit during an upgrade. Below is my attempt at figuring out the
answer.

{% gist 8843744 %}

You can try changing the code before an upgrade message is received
and see for real that hot-code reloading indeed works!  Also, since
the client is sending 10 pings, it must receive 10 pongs. Does this
guarantee that no pings are lost?

Here is a demo in my terminal showing the execution of the above
program. During the execution before the 5th ping, I uncomment the
code at line 29 to display the new version running message after an
upgrade has occurred.

{% showterm 0159350f205cc475c7f18 %}

In erlang, the vm is a huge advantage since it controls the execution
and keeps multiple versions of modules in memory.  Besides, it can
make sure that some calls go to the current module and some go to the
old module.  Also, the mailbox of each process is preserved during
upgrades.

How to achieve this in Cloud Haskell?
