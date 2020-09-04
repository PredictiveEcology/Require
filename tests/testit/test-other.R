# Test misspelled
out <- tryCatch(Require("data.tt"), warning = function(w) w)
testit::assert(is(out, "simpleWarning"))

