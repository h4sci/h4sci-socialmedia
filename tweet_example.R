library(quanteda)
library(rtweet)
library(dplyr)
library(quanteda)
library(ggplot2)

# silver_tweet <- search_tweets(
#   "Nate Silver",
#   n = 30000, include_rts = FALSE,
#   retryonratelimit = TRUE
# )
load("data/oct_nov_silver.RData")

election <- silver_tweet %>%
  filter(created_at > as.POSIXct("2020-11-03") &
           created_at < as.POSIXct("2020-11-08"))


election_early <- silver_tweet %>%
  filter(created_at > as.POSIXct("2020-11-03") &
           created_at < as.POSIXct("2020-11-05"))

election_late <- silver_tweet %>%
  filter(created_at > as.POSIXct("2020-11-06") &
           created_at < as.POSIXct("2020-11-08"))


create_quanteda_obj <- function(df,
                                stops = c("Nate",
                                          "Silver",
                                          "Silver's",
                                          "like",
                                          "just",
                                          "@natesilver538",
                                          "Trump",
                                          "Biden")){
  out <- list()
  tweet_corp <- corpus(df, text_field = "text")
  tweet_tok <- tweet_corp %>%
    tokens(remove_punct = TRUE,
           remove_numbers = TRUE,
           remove_url = TRUE) %>%
    tokens_remove(c(stops,
                    stopwords("en")))

  tweet_dfm <- tweet_tok %>%
    dfm()

  trimmed <- tweet_dfm %>%
    dfm_trim(min_termfreq = 60, verbose = FALSE)

  dfm_weight <- tweet_dfm %>%
    dfm_weight(scheme = "prop")

  out$corp <- tweet_corp
  out$tok <- tweet_tok
  out$dfm <- tweet_dfm
  out$trimmed <- trimmed
  out$dfm_weight <- dfm_weight
  out
}

# trigrams
early_nate <- create_quanteda_obj(election_early,
                                  stops = c("like","just"))
late_nate <- create_quanteda_obj(election_late,
                                 stops = c("like","just"))


early_tri <- tokens_ngrams(early_nate$tok, 4) %>%
  dfm()
late_tri <- tokens_ngrams(late_nate$tok, 4) %>%
  dfm()



fw_e <- textstat_frequency(early_tri,
                                  n = 20)
ggplot(data = fw_e,
       aes(x = nrow(fw_e):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(fw_e):1,
                     labels = fw_e$feature) +
  labs(x = NULL, y = "Relative frequency")


fw_l <- textstat_frequency(late_tri,
                           n = 20)
ggplot(data = fw_l,
       aes(x = nrow(fw_l):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(fw_l):1,
                     labels = fw_l$feature) +
  labs(x = NULL, y = "Relative frequency")





# Objects w/o names early
early <- create_quanteda_obj(election_early)
late <- create_quanteda_obj(election_late)
textplot_wordcloud(early$trimmed,max_words = 100)
textplot_wordcloud(late$trimmed,max_words = 100)

# Top Hash Tags
tag_dfm <- dfm_select(early$dfm, pattern = ("#*"))
toptag <- names(topfeatures(tag_dfm, 50))

tag_dfm <- dfm_select(late$dfm, pattern = ("#*"))
toptag <- names(topfeatures(tag_dfm, 50))

tag_fcm <- fcm(tag_dfm)
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)


# some LDA
library(quanteda.textmodels)
dfmat_news <- early_nate$dfm %>%
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")
dfmat_news <- dfmat_news[ntoken(dfmat_news) > 0,]
# runs semisupervised Latent Dirichlet allocation
tmod_lda <- textmodel_lda(dfmat_news, k = 5)
head(tmod_lda$theta)


tmod_lda$theta[,3][tmod_lda$theta[,3] > 0.5]

texts(early_nate$corp)[8057]
texts(early_nate$corp)[8031]
texts(early_nate$corp)[7915]
texts(early_nate$corp)[7707]

tmod_lda$theta[,2][tmod_lda$theta[,2] > 0.7]
texts(early_nate$corp)[15165]
texts(early_nate$corp)[15099]

# terms(tmod_lda, 5)
# head(topics(tmod_lda), 20)
# dfmat_news$topic <- topics(tmod_lda)
#
# table(dfmat_news$topic)
# terms(tmod_lda, 10)
#
# head(early_tri)


dfmat_news <- late_nate$dfm %>%
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")
dfmat_news <- dfmat_news[ntoken(dfmat_news) > 0,]
# runs semisupervised Latent Dirichlet allocation
tmod_lda <- textmodel_lda(dfmat_news, k = 5)
terms(tmod_lda,5)

tmod_lda$theta[,3][tmod_lda$theta[,3] > 0.8]
texts(late_nate$corp)[228]
texts(late_nate$corp)[689]




