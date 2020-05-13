# GeneHive R package

## Getting started

### 1. Set environment variables
When the GeneHive package is loaded, several R session options are set based on the following environment variables:
- **`HIVE_USERNAME`** The user name to be used to connect to the GeneHive instance. If not specified, `Sys.info()["user"]` will be used.
- **`HIVE_HOSTNAME`** The name of the GeneHive server. If not specified, `"localhost"` will be used.
- **`HIVE_HTTPS`** Set to `true` for HTTPS traffic or `false` for HTTP traffic. If not specified, HTTPS traffic will be used.

In a Unix-like system (including Macs), these may be set with the `export` command.

For example, the following lines might be added to a `~/.bashrc` file:
```
# GeneHive environment variables
export HIVE_USERNAME="agower"
export HIVE_HOSTNAME="genehive.bu.edu"
export HIVE_HTTPS=true
```
In a Windows environment, these may be set through the Environment Variables system dialog (accessible by searching the Control Panel or Start menu).

### 2. Install the uuidtools and GeneHive packages
The GeneHive package and its dependency uuidtools may be installed directly from GitHub with the commands:
```
devtools::install_github("agower/uuidtools")
devtools::install_github("agower/GeneHive")
```

### 3. Change the password
You will receive a temporary password from the administrator of the GeneHive instance when your account is created.

You should replace this password with a strong, random password using the commands:
```
library(GeneHive)
changePassword()
```

By default, the `changePassword()` function performs the following actions:
- Generate a random, strong password, using `/dev/urandom` (if available) or the R random number generator otherwise
- Add the current hostname, username, and random password to `~/.netrc` (in Unix-like systems) or `~/_netrc` (in Windows).

The [.netrc file format](https://ec.haxx.se/usingcurl/usingcurl-netrc) is simple: each line contains a separate whitespace-delimited entry describing a machine, username, and password. For example, a typical entry might look like:
```
machine genehive.bu.edu login agower password Fbyd0+TQFPLjuGEU_4vd
```

### 4. Test the credentials
The simplest way to test whether a connection can be made to a GeneHive is to issue a command to list all visible users.  As this will always include at least the user performing the query, it should always return a result, regardless of the state of the hive, and is therefore a useful quick check for connectivity.

For example:
```
> listUsers()
  username     group    groups superuser         email firstName lastName  ...  active
1   agower rootgroup rootgroup      TRUE agower@bu.edu      Adam    Gower         TRUE
```
