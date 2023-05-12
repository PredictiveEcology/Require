setupInitial <- setupTest()

library(Require)

a <- extractPkgName("Require (>=0.0.1)")
testit::assert({
  isTRUE(all.equal("Require", a))
})
a <- extractPkgName("PredictiveEcology/Require (>=0.0.1)")
testit::assert({
  isTRUE(all.equal("Require", a))
})

a <- extractVersionNumber("Require (<=0.0.1)")
testit::assert({
  isTRUE(all.equal("0.0.1", a))
})
a <- extractVersionNumber("PredictiveEcology/Require (>=0.0.1)")
testit::assert({
  isTRUE(all.equal("0.0.1", a))
})

a <- extractInequality("Require (<=0.0.1)")
testit::assert({
  isTRUE(all.equal("<=", a))
})
a <- extractInequality("Require (==0.0.1)")
testit::assert({
  isTRUE(all.equal("==", a))
})
a <- extractInequality("Require (>=0.0.1)")
testit::assert({
  isTRUE(all.equal(">=", a))
})

a <- extractPkgGitHub("PredictiveEcology/Require")
testit::assert({
  isTRUE(all.equal("Require", a))
})
a <- extractPkgGitHub("PredictiveEcology/Require (>=0.0.1)")
testit::assert({
  isTRUE(all.equal("Require", a))
})
a <- extractPkgGitHub("Require (>=0.0.1)")
testit::assert({
  identical(is.na(NA), is.na(a))
}) # Seems to be different class under different conditions

a <- trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
testit::assert({
  isTRUE(all.equal("PredictiveEcology/Require", a))
})
a <- trimVersionNumber("Require (<=0.0.1)")
testit::assert({
  isTRUE(all.equal("Require", a))
})

out <- parseGitHub("r-forge/mumin/pkg")
testit::assert({
  "hasSubFolder" %in% colnames(out)
})


endTest(setupInitial)
