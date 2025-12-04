app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]
import "input.txt" as raw_instructions : Str

main! : List Arg => Result {} _
main! = |_args|
    parsed_instructions = raw_instructions
        |> Str.split_on("\n")
        |> List.drop_if((|elem| Str.is_empty(elem)))
        |> List.map(|elem| Str.replace_first(elem, "R", ""))
        |> List.map(|elem| Str.replace_first(elem, "L", "-"))
        |> List.map(|elem| Str.to_i64(elem))
        |> List.map(|elem| when elem is 
            Ok(value) -> value
            Err(_) -> crash "Wuh Woh"
        )
    solution = List.walk(parsed_instructions, {position: 50, zeroes: 0, clicks: 0, index: 0 }, | state, instruction |
        new_position = (100 + (state.position + instruction)) % 100
        new_zeroes = state.zeroes + Num.from_bool(new_position == 0)
        new_clicks = state.clicks 
            + Num.floor(Num.to_f64(Num.abs(instruction)) / 100) 
            + Num.from_bool(state.position != 0 && (Num.abs(state.position) < new_position))
            + Num.from_bool(new_position != 0 && (Num.abs(state.position) > new_position))
        dbg state
        dbg instruction
        { position: new_position, zeroes: new_zeroes, clicks: new_clicks, index: state.index + 1}
    )
    _ = Stdout.line!("num_zeroes: ${Num.to_str(solution.zeroes)}")
    Stdout.line!("num_clicks: ${Num.to_str(solution.clicks)}")
