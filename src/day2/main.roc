app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]
import "input.txt" as id_raw : Str

is_valid: Frac F64 -> Bool
is_valid = | number |
    digits = Num.to_frac(Num.ceiling(Num.log(number) / Num.log(10)))
    upper = Num.floor(number / Num.pow(10, digits/2))
    lower = Num.floor(number) % Num.floor(Num.pow(10, digits/2))
    upper != lower

is_more_valid: Frac F64 -> Bool
is_more_valid = | number |
    digits = Num.ceiling(Num.log(number) / Num.log(10))
    divisors = List.keep_if(List.range({start: At 1, end: Before digits}), | x | digits % x == 0)
    pattern_finder = | patterns_found, divisor |
        split = List.map(List.range({start: At 0, end: Before digits, step: divisor}), | x | Num.floor(number / 10^Num.to_frac(x)) % Num.round(10^Num.to_frac(divisor))) 
        first = when List.first(split) is
            Ok(value) -> value
            Err(_) -> crash "Wuh Woh"
        has_pattern = List.all(split, |elem| elem == first)
        patterns_found + Num.from_bool(has_pattern && first != 0)
    num_patterns = List.walk(divisors, 0, pattern_finder)
    num_patterns == 0

main! : List Arg => Result {} _
main! = |_args|
    id_ranges = id_raw
        |> Str.replace_each("\n", "")
        |> Str.split_on(",")
        |> List.drop_if((|elem| Str.is_empty(elem)))
        |> List.map(|elem| 
            x = List.map(Str.split_on(elem, "-"), |s| 
                when Str.to_i64(s) is
                    Ok(value) -> value
                    Err(_) -> crash "Wuh Woh"
            )
            first = when List.first(x) is 
                Ok(value) -> value
                Err(_) -> crash "Wuh Woh"
            last = when List.last(x) is 
                Ok(value) -> value
                Err(_) -> crash "Wuh Woh"
            List.range({ start: At first, end: At last })
        )

    valid_total = List.walk(id_ranges, 0, | count, id_range | 
        count + List.walk(id_range, 0, | subtotal, value | 
            x = if is_valid(Num.to_frac(value)) then 0 else value
            subtotal + x
        ))
    more_valid_total = List.walk(id_ranges, 0, | count, id_range | 
        count + List.walk(id_range, 0, | subtotal, value | 
            x = if is_more_valid(Num.to_frac(value)) then 0 else value
            subtotal + x
        ))
    _ = Stdout.line!("${Num.to_str(valid_total)}")
    Stdout.line!("${Num.to_str(more_valid_total)}")
