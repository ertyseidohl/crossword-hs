# Crossword Tool

This is a crossword editing tool written in Haskell.

It uses a somewhat brute-force approach for solving crossword puzzles (no 3-SAT solver or anything, just like... try every combination until one works). Fortunately the laziness of Haskell lets us get away with this.

I've had it solve empty 5x5 puzzles in a reasonable amount of time, but the time increases exponentially, so YMMV with larger systems.

## Wordlist

The wordlist is based on a historical dump of NYT crossword answers, arranged by number of appearances in puzzles.

If you put more TSV files in the `data` folder, the program will automatically pick them up and include them as part of the wordlist.

## Server

This uses Stack. You can run the server with `stack run server`. It will start serving on $PORT or :8081.

I've temporarily hard-coded the CORS value to be my heroku server, you'll need to replace that if you want to run it locally.

## CLI Tool

You can also run a command line tool with `stack run crossword-repl`.

The CLI tool will wait for you to enter a crossword, like so:

```
AB#
...
#..
```

Where `#` is a dark cell and `.` is an empty cell. Enter two blank lines after your input, and the crossword tool will attempt to fill in all of the blank cells.

## Future work

There's plenty of work to do and I welcome PRs. Some ideas:

- Fix the static CORS address to be an env var.
- Parallelize the solver to get a speedup when there are multiple cores.
- Rewrite the solver to use SAT-solving strategies instead of brute force.
- Be able to tell the CLI tool that you didn't like one of the words it chose, so try again without it.

