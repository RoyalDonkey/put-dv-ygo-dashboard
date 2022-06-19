# This script computes a few large dataframes. These are saved
# to external files and loaded to take the load off the server.

# Load data
cards    <- read.csv('data/cards/cards.csv')
variants <- read.csv('data/cards/variants.csv')
decks    <- fromJSON(file='data/decks/decks.json')

# Cards cache - fetch card information based on its id in O(1) time
cards_cache   <- new.env(hash=TRUE, parent=emptyenv())
decks_contain <- new.env(hash=TRUE, parent=emptyenv())
for (i in 1:nrow(cards)) {
  card <- cards[i,]
  assign(paste0('c', card$id), card, envir=cards_cache)
  assign(paste0('c', card$id), 0, envir=decks_contain)
}
rm(card)

# Function to obtain card metadata from its ID code in O(1) time
getCard <- function(id) {
  return(cards_cache[[paste0('c', id)]])
}

# Function to obtain the number of decks a card appears in in O(1) time
getCardDecks <- function(id) {
  return(decks_contain[[paste0('c', id)]])
}

# Cache list of all cards in all decks and a data frame of the number of decks
# containing each card.
decks_cards <- c()
#i <- 0
for (deck in decks) {
  for (card_id in deck$deck$main) {
    # If it's a variant, convert it to parent; we don't care about variants
    parent <- variants[variants$variant == card_id,]$parent
    if (length(parent) != 0) {
      card_id <- parent
    }
    
    # If ID is invalid (noisy data), skip it
    if (!is.null(get0(paste0('c', card_id), envir=decks_contain))) {
      assign(paste0('c', card_id),
            get(paste0('c', card_id), envir=decks_contain) + 1,
            envir=decks_contain)
      decks_cards[length(decks_cards) + 1] <- list(getCard(card_id)[c('id', 'type')])
    }
  }
  #i <- i + 1
  #print(paste(i, '/', length(decks)))
}

#Convert list of decks to convenient data table with required information in columns
for (i in 1:length(decks)) {
  decks[[i]][[length(decks[[i]])]] <- NULL 
}
rm(i)
deckbind <- rbindlist(decks, fill=TRUE)

rm(deck)
decks_cards <- as.data.frame(do.call(rbind, decks_cards))
DECKS_CARDS_COUNT <- nrow(decks_cards)
decks_cards <- setDT(decks_cards)[, .N, decks_cards]
DECKS_COUNT <- length(decks)

save(cards_cache,       file='cards_cache.Rdata')
save(decks_contain,     file='decks_contain.Rdata')
save(decks_cards,       file='decks_cards.Rdata')
save(DECKS_CARDS_COUNT, file='DECKS_CARDS_COUNT.Rdata')
save(DECKS_COUNT,       file='DECKS_COUNT.Rdata')
save(deckbind,          file='deckbind.Rdata')
