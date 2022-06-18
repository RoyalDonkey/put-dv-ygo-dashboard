# This script computes a few large dataframes. These are saved
# to external files and loaded to take the load off the server.

# Load data
cards <- read.csv('cards.csv')
decks <- fromJSON(file='decks.json')

# Cards cache - fetch card information based on its id in O(1) time
cards_cache <- new.env(hash=TRUE, parent=emptyenv())
for (i in 1:nrow(cards)) {
  card <- cards[i,]
  assign(paste0('c', card$id), card, envir=cards_cache)
}
rm(card)
save(cards_cache, file='cards_cache.Rdata')

# Cache list of all cards in all decks
# Build a single list of all cards across all decks
decks_cards <- c()
for (deck in decks) {
  for (card_id in deck$deck$main) {
    decks_cards[length(decks_cards) + 1] <- list(getCard(card_id)[c('id', 'type')])
  }
}
rm(deck)
decks_cards <- as.data.frame(do.call(rbind, decks_cards))
DECKS_CARDS_COUNT <- nrow(decks_cards_rows)
decks_cards <- setDT(decks_cards)[, .N, decks_cards]
save(decks_cards, file='decks_cards.Rdata')
save(DECKS_CARDS_COUNT, file='DECKS_CARDS_COUNT.Rdata')
