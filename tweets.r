setwd('C:/Users/Bensabat Elliot/Desktop/tweets')
library(base64enc)
library(httr)
library(RCurl)
library(ROAuth)
library(streamR)
library(twitteR)
library(translateR)
library(tm)
library(lda)
library(LDAvis)
library(servr)
library(wordcloud)

#y <- read.csv("testTweets.csv", header = TRUE)

#download certificate needed for authentication, creates a certificate file on desktop
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

refresh <- function() {
  # Twitter configuration
  # Set all the Configuration details to authorize your application to access Twitter data.
  requestURL        <- "https://api.twitter.com/oauth/request_token"
  accessURL         <- "https://api.twitter.com/oauth/access_token"
  authURL           <- "https://api.twitter.com/oauth/authorize"
  consumerKey       <- "DYptuYXGNGMPrgF1fsIytrDjn"
  consumerSecret    <- 	'XXX'
  accessToken       <- 	'XXX'
  accessTokenSecret <- 'XXX'
  
  #obtain oauth by handshaking and save the oauth to the local disk for future connections
  #my_oauth <- OAuthFactory$new( consumerKey=consumerKey,
  #                             consumerSecret=consumerSecret,
  #                            requestURL=requestURL,
  #                           accessURL=accessURL, 
  #                          authURL=authURL)
  
  # returns the oauth 
  #my_oauth$handshake(cainfo="cacert.pem")
  
  setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
  
  x <- searchTwitter("Topic",1000)
  y <<- twListToDF(x)
}

#------------------------------------------------------------------------------------------


  stopWords <- "rt, ht, co, bcg, https, http, RT, a, about, above, across, after, again, against, all, almost, alone, along, already, also, although, always, am, among, an, and, another, any, anybody, anyone, anything, anywhere, are, area, areas, aren't, around, as, ask, asked, asking, asks, at, away, b, back, backed, backing, backs, be, became, because, become, becomes, been, before, began, behind, being, beings, below, best, better, between, big, both, but, by, c, came, can, cannot, can't, case, cases, certain, certainly, clear, clearly, come, could, couldn't, d, did, didn't, differ, different, differently, do, does, doesn't, doing, done, don't, down, downed, downing, downs, during, e, each, early, either, end, ended, ending, ends, enough, even, evenly, ever, every, everybody, everyone, everything, everywhere, f, face, faces, fact, facts, far, felt, few, find, finds, first, for, four, from, full, fully, further, furthered, furthering, furthers, g, gave, general, generally, get, gets, give, given, gives, go, going, good, goods, got, great, greater, greatest, group, grouped, grouping, groups, h, had, hadn't, has, hasn't, have, haven't, having, he, he'd, he'll, her, here, here's, hers, herself, he's, high, higher, highest, him, himself, his, how, however, how's, i, i'd, if, i'll, i'm, important, in, interest, interested, interesting, interests, into, is, isn't, it, its, it's, itself, i've, j, just, k, keep, keeps, kind, knew, know, known, knows, l, large, largely, last, later, latest, least, less, let, lets, let's, like, likely, long, longer, longest, m, made, make, making, man, many, may, me, member, members, men, might, more, most, mostly, mr, mrs, much, must, mustn't, my, myself, n, necessary, need, needed, needing, needs, never, new, newer, newest, next, no, nobody, non, noone, nor, not, nothing, now, nowhere, number, numbers, o, of, off, often, old, older, oldest, on, once, one, only, open, opened, opening, opens, or, order, ordered, ordering, orders, other, others, ought, our, ours, ourselves, out, over, own, p, part, parted, parting, parts, per, perhaps, place, places, point, pointed, pointing, points, possible, present, presented, presenting, presents, problem, problems, put, puts, q, quite, r, rather, really, right, room, rooms, s, said, same, saw, say, says, second, seconds, see, seem, seemed, seeming, seems, sees, several, shall, shan't, she, she'd, she'll, she's, should, shouldn't, show, showed, showing, shows, side, sides, since, small, smaller, smallest, so, some, somebody, someone, something, somewhere, state, states, still, such, sure, t, take, taken, than, that, that's, the, their, theirs, them, themselves, then, there, therefore, there's, these, they, they'd, they'll, they're, they've, thing, things, think, thinks, this, those, though, thought, thoughts, three, through, thus, to, today, together, too, took, toward, turn, turned, turning, turns, two, u, under, until, up, upon, us, use, used, uses, v, very, w, want, wanted, wanting, wants, was, wasn't, way, ways, we, we'd, well, we'll, wells, went, were, we're, weren't, we've, what, what's, when, when's, where, where's, whether, which, while, who, whole, whom, who's, whose, why, why's, will, with, within, without, won't, work, worked, working, works, would, wouldn't, x, y, year, years, yes, yet, you, you'd, you'll, young, younger, youngest, your, you're, yours, yourself, yourselves, you've, z"
  stopWords <- unlist(strsplit(stopWords, ", "))
  stop_words <- data.frame(matrix(unlist(stopWords)))
  colnames(stop_words)[1] <- "word"
  
  
  tweets  <- as.character(y$text)
  
  stop_words1 <- as.character(stop_words$word)
  
  tweets <- gsub("'", "", tweets)  # remove apostrophes
  tweets <- gsub("[[:punct:]]", " ", tweets)  # replace punctuation with space
  tweets <- gsub("[[:cntrl:]]", " ", tweets)  # replace control characters with space
  tweets <- gsub("^[[:space:]]+", "", tweets) # remove whitespace at beginning of documents
  tweets <- gsub("[[:space:]]+$", "", tweets) # remove whitespace at end of documents
  tweets <- gsub("[^[:alnum:]///' ]", "", tweets) # remove special characters
  tweets <- tolower(tweets)  # force to lowercase
  tweets <- gsub("d+", "", tweets) # remove numbers
  
  
  
  # tokenize on space and output as a list:
  doc.list <- strsplit(tweets, "[[:space:]]+")
  
  # compute the table of terms:
  term.table <- table(unlist(doc.list))
  term.table <- sort(term.table, decreasing = TRUE)
  
  # remove terms that are stop words or occur fewer than 3 times:
  del <- names(term.table) %in% stop_words1 | term.table < 3
  term.table <<- term.table[!del]
  vocab <<- names(term.table)
  
  # now put the documents into the format required by the lda package:
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  documents <<- lapply(doc.list, get.terms)
  
  D <- length(documents) 
  W <- length(vocab)  
  doc.length <<- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
  N <- sum(doc.length)  # total number of tokens in the data
  term.frequency <<- as.integer(term.table)
  
 
  K <- 10
  G <- 5000
  alpha <- 0.02
  eta <- 0.02
  
  # Fit the model:
  
  set.seed(357)
  t1 <- Sys.time()
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  t2 <- Sys.time()
  t2 - t1  
  
  
  theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
  
  tweetTopics <<- list(phi = phi,
                      theta = theta,
                      doc.length = doc.length,
                      vocab = vocab,
                      term.frequency = term.frequency)


  # create the JSON object to feed the visualization:
  json <- createJSON(phi = tweetTopics$phi, 
                     theta = tweetTopics$theta, 
                     doc.length = tweetTopics$doc.length, 
                     vocab = tweetTopics$vocab, 
                     term.frequency = tweetTopics$term.frequency)
  
  
  serVis(json, out.dir = 'vis', open.browser = TRUE)



