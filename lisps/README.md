## Installation

SBCL, CCL and ECL are supported as host environments.
To install executable that wraps lisp implementation for scripting:

```sh
# if /usr/local/bin/ is in PATH and writable by a user
./install-lisp-script.sh sbcl /usr/local/bin/lisps
```

## Usage

```sh
#!/usr/bin/env lisps

(print 'hello)
```
