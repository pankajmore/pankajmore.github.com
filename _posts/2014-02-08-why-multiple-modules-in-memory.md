---
layout: post
title:  "Why do we need multiple versions of a module in memory?"
categories: thesis erlang cloud haskell
comments: true
---

We can spawn multiple processes on a single node. Both erlang and
cloud-haskell support this feature.

Can we force a process to upgrade at any arbitrary point in time?

Think about a scenario when a process is writing to a file/socket and
acquiring lock to a shared resource (for example printing a file). We
would not like the process to abruptly upgrade.

So, how do we upgrade a process?

We send an upgrade message to the process. When the process is ready,
it can pick the message from its mailbox, and upgrade
safely. Processes can now upgrade at different points in time. Imagine
multiple processes running in a single node. Each process can be
running a different version of a module. We cannot keep infinite
number of versions of a module in memory. Erlang only supports 2
simultaneous versions per module. When a third version is loaded into
memory, the oldest version is removed and processes running the oldest
version simply crash. It improves reliability in the sense that at
least a single upgrade doesn't crash other processes in a node.

Keeping 2 versions of a module in memory is then an issue of enhancing
reliability. Even if we have only one version in memory, it still is
not bad enough. The only caveat is that other local processes might
crash.
