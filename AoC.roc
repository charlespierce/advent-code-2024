module [solve]

import pf.Dir
import pf.File
import pf.Http
import pf.Stdout
import pf.Utc

Solution ans1 ans2 : {
    day : U8,
    part1 : Str -> ans1,
    part2 : Str -> ans2,
} where ans1 implements Inspect, ans2 implements Inspect

solve : U16, Solution _ _ -> Task {} _
solve = \year, solution ->
    input = getInput! { year, day: solution.day }

    startPart1 = Utc.now! {}
    part1 = solution.part1 input
    endPart1 = Utc.now! {}
    part2 = solution.part2 input
    endPart2 = Utc.now! {}

    part1Duration = formatDuration startPart1 endPart1
    part2Duration = formatDuration endPart1 endPart2

    Stdout.line! "AOC $(Num.toStr year) - Day $(Num.toStr solution.day)"
    Stdout.line! "        Part 1 : $(Inspect.toStr part1)"
    Stdout.line! "        runtime: $(part1Duration)"
    Stdout.line! ""
    Stdout.line! "        Part 2 : $(Inspect.toStr part2)"
    Stdout.line! "        runtime: $(part2Duration)"

formatDuration : Utc.Utc, Utc.Utc -> Str
formatDuration = \start, end ->
    nanos = Utc.deltaAsNanos start end

    if nanos > 1_000_000 then
        millis = Num.divCeil nanos 1_000_000
        "$(Num.toStr millis)ms"
    else if nanos > 1_000 then
        micros = Num.divCeil nanos 1_000
        "$(Num.toStr micros)μs"
    else
        "$(Num.toStr nanos)ns"

Puzzle : { year : U16, day : U8 }

getInput : Puzzle -> Task Str _
getInput = \puzzle ->
    Task.onErr (readInputCache puzzle) \_ ->
        input = fetchInput! puzzle
        writeInputCache! puzzle input
        Task.ok input

inputCacheDir : Puzzle -> Str
inputCacheDir = \{ year } -> "input/$(Num.toStr year)"

inputCacheFile : Puzzle -> Str
inputCacheFile = \puzzle -> "$(inputCacheDir puzzle)/$(Num.toStr puzzle.day).txt"

readInputCache : Puzzle -> Task Str _
readInputCache = \puzzle -> File.readUtf8 (inputCacheFile puzzle)

writeInputCache : Puzzle, Str -> Task {} _
writeInputCache = \puzzle, input ->
    Dir.createAll! (inputCacheDir puzzle)

    File.writeUtf8! (inputCacheFile puzzle) input

fetchInput : Puzzle -> Task Str _
fetchInput = \puzzle ->
    response =
        prepareInputRequest! puzzle
        |> Http.send!

    response.body
    |> Str.fromUtf8
    |> Task.fromResult

prepareInputRequest : Puzzle -> Task Http.Request _
prepareInputRequest = \puzzle ->
    sessionCookie = File.readUtf8! ("credential.txt")
    url = "https://adventofcode.com/$(Num.toStr puzzle.year)/day/$(Num.toStr puzzle.day)/input"
    headers =
        List.single (Http.header "cookie" "session=$(sessionCookie)")
        |> List.append (Http.header "user-agent" "github.com/charlespierce/advent-code-2024 by chuck@charlespierce.dev")
    Task.ok { Http.defaultRequest & url, headers }
