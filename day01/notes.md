# setup
Using cargo was pretty straightforward. just use new or init, cargo build/run, I could easily find something like ghcid (cargo-watch) which was equally as easy to use.
Being able to just write `#[test]` directives (I think that's how they call it) in whatever source file you want, and `cargo test` was pretty satisfying. Really cool feature.
I guess I understand why people praise rust's tooling.

# part 1
2018 aoc's day 1 part 1 is pretty easy, just parse numbers separated by newline.
Nothing fancy here, rust's std has a parse function that even parses the sign (+/-).
Reading files is pretty classic too : open, make a buffer reader and then iterate over `lines()`

I used `panic!` in the parser function, figured that was the most straightforward.

# part 2

This was a lot more painful. First of all, Monads and typeclasses are great, they're really great. This is probably very subjective, but threading `Option`s and `Result`s really made me realize how convenient do-notation or even just th:e various FAM-related operators are! I really don't like how often I had to litter my code with `.unwrap()`...

I didn't really get the appeal of the `?` operator. I just prefered to manually check if I had an error and use `std::process::exit()`. The point being to get the pure value or fail.

I wasted a huge time on getting the lines of the file to work with `.cycle()`. In the end I settled on collecting a `Vec<String>` and using that.

Since I didn't really get the whole Iterators hierarchy or whatever, I thought it was a good idea to let the type inference handle this for me and wrote `fn day2<I>(xs: Cycle<I<i32>>) -> i32` (Maybe it should have been `<I : impl Iterator>` or something like that though). But then we get a `type arguments are not allowed for this type`, but unfortunately rust doesn't support HKTs or similar.

I have to admit I still don't understand how borrowing really works, whenever I met an error related to it, I would just rewrite so I get something more functional. I also don't really get why `HashSet::contains` takes a reference, I mean aren't the elements supposed to implement Eq and Hash?
