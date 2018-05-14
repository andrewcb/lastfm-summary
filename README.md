# lastfm-summary

lastfm-summary is a command-line program for generating short textual reports of a user's [Last.fm](https://last.fm) feed. It gets the relevant data from Last.fm's API, formats it into text, and emits it to standard output. It is intended to be used with scripts for sending or posting these summaries to somewhere interesting (i.e., potentially as a self-hosted version of the [tweekly.fm](http://tweekly.fm) service, targeting destinations of the user's choosing).

## Building

lastfm-summary is implemented in Haskell using the [Stack](https://haskellstack.org) build system. To build and install in your local binary path, enter `stack install` in the project directory; this will automatically fetch and build all dependencies.

## Installing

lastfm-summary needs a configuration file, which resides in a hidden directory in the user's home directory, and is named `~/.lastfm-summary/config`. This must contain at least a Last.fm API key (obtainable from last.fm). This may also contain a last.fm username, which will be used if no username is specified on the command line. A configuration file may look something like:

```
username=yournamehere
api_key=01234567012345670123456701234567
```

### Configuration options

The `config` file may contain the following options:

- **api_key** (required) - the last.fm API key; obtain this from [last.fm's website](https://www.last.fm/api/account/create).
- **username** (optional) - the default last.fm username of the account whose feeds to summarise. This may be overridden with the `-u` command-line option.

## Running

This program has one mandatory argument, which specifies what it must summarise. This may be one of:

- **topartists** - A list of the most-played artists in the last period, with play counts.
- **toptracks** - A list of the most-played tracks in the last period, with play counts.
- **topalbums** - A list of the most-played albums in the last period, with play counts.
- **recent** - A list of the tracks played most recently

The output of these can be controlled with these command-line options:

- **-p** *period* - specify a period (for top artists/tracks/albums); this must be one of last.fm's standard periods, `week` (the last 7 days), `month` (the last month), `3m`, `6m` or `12m`, or `overall` (of all time). Ignored for most-recent items.
- **-l** *N* - specify the number of items to list. This means list the top N items, or the N most recent items.
- **-u** *username* - specify the last.fm username to query.

## Implementation details

`lastfm-summary` uses the [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative) package for command-line parsing, the [ConfigFile](http://hackage.haskell.org/package/ConfigFile) package for configuration file parsing and the [Aeson](https://hackage.haskell.org/package/aeson) package for parsing last.fm's JSON output.

## Current limitations

`lastfm-summary` currently only implements these four API endpoints (though more should be straightforward to add). It currently has no way of querying recently played tracks by cut-off dates. The output formats are currently not configurable other than by editing the code. There is not yet any facility for doing anything with the output other than printing it to `stdout`; beyond that, you're on your own. Finally, this program's name is a bit unimaginative.

## Author

`lastfm-summary` was written by [Andrew Bulhak](http://dev.null.org/acb/).
