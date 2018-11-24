# nick linguistic generator

badNouns <- c("noob","pansy","loser", "You", "Failure","Rob","fool","idiot")
goodNouns <- c("beast","I","Nick","The slayer")
verbs <- c("will pown", "vanquish the", "is", "was", "were", "are", "owned the",
  "logic powned","slay","slayed the","powned", "am", "was", "been", "being", "be")
goodAdjectives <- c("epic","sick","beastly","on a stick","spiffy","magnificent")
badAdjectives <- c("noobish","pansyish","slayed","","fat","obese","silly",
  "stupid")
goodAdverbs <- c("epically","")
badAdverbs <- c("noobishly","","foolishly")

P_goodbad <- .5

sampleWord <- function(x)
{
  sample(x, size=1, replace=TRUE)
}

sampleSaying <- function()
{
  goodbad <- sample(c("good","bad"),size=1, replace=TRUE, prob=c(P_goodbad,1-P_goodbad))
  if (goodbad == "good")
  {
    subject <- sampleWord(goodNouns)
    verb <- sampleWord(verbs)
    while (subject == "I" && verb %in% c("is","were","are","be","been","being"))
    {
      verb <- sampleWord(verbs)
    }
    adverb <- sampleWord(goodAdverbs)
    adjective <- sampleWord(goodAdjectives)
    object <- sampleWord(goodNouns)
    while (subject == object || object == "I")
    {
      object <- sampleWord(goodNouns)
    }
  } else
  {
    subject <- sampleWord(badNouns)
    if (subject == "I")
    {
      verb <- sampleWord(personalVerbs)
    } else verb <- sampleWord(verbs)
    adverb <- sampleWord(badAdverbs)
    adjective <- sampleWord(badAdjectives)
    object <- sampleWord(badNouns)
    while (subject == object || object == "I")
    {
      object <- sampleWord(badNouns)
    }
  }
  print(paste(subject, verb, adverb, adjective, object))
}

sampleSaying()
