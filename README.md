# pscheme

pscheme is an executable-producing ahead-of-time [r7rs](https://small.r7rs.org/)
[scheme](https://en.wikipedia.org/wiki/Scheme_\(programming_language\))
compiler. it is the successor to [plisp2](https://github.com/petelliott/plisp2),
and is availible under the GNU [LGPLv3](/COPYING.LESSER).

## running

until it is self hosted, pscheme is bootstrapped by
[gauche](http://practical-scheme.net/gauche/download.html), although porting it
to other r7rs implementations should be trivial.

```bash
$ make
$ ./pscheme your-file.scm -I /your/load/path/
```