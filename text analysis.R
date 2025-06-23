library(tidyverse)
library(tidytext)
library(sentimentr)
library(tm)
library(SnowballC)
library(wordcloud)
library(lexicon)
library(igraph)
library(reshape2)




#global options for bigram network plots
igraph_options(vertex.size=30, 
							 vertex.frame.color = "white",
							 vertex.color = "gray95", 
							 vertex.label.color = "black",
							 edge.color = "gray80", 
							 vertex.label.cex=1
)






# functions -----------------------------------------------------------



#need to integrate negations
# str_extract(responses, '(?<=not\\s)\\w+')   finds the word occuring after "not "
# str_extract(responses, '(?<=no\\s)\\w+')   finds the word occuring after "no "


preprocess_text = function(x, remove_stopwords = T, remove_suffixes = T,
													 remove_whitespace = T){
	
	x = paste0(" ", x, " ")
	
	res = x %>%
		tolower(.) %>%
		
		#line breaks to periods
		gsub("[[:cntrl::]", "\\.", .)  %>%
		
		
		#take care of contractions
		gsub(" ([[:alpha:]]*?)\\'([[:alpha:]]*?) ", " \\1\\2 ", .) %>%
		
		# remove a URL and punctuation
		gsub("\\( see https.*?\\)\\.", "", .) %>%
		gsub("[[:punct:]]", " ", .) %>%
		
		gsub(" office hours ", " officeHours ", .) %>%
		gsub(" department chair ", " departmentChair ", .) %>%
		gsub(" in person ", " inPerson ", .) %>%
		gsub(" full time ", " fullTime ", .) %>%
		gsub(" part time ", " partTime ", .) %>%
		gsub(" ft ", " fullTime ", .) %>%
		gsub(" pt ", " partTime ", .) %>%
		gsub(" office space ", " officeSpace ", .) %>%
		gsub(" work space ", " workspace ", .) %>%
		gsub(" individual office ", " private office ", .) %>%
		gsub(" enclosed office ", " private office ", .) %>%
		gsub(" close proximity ", " close ", .) %>%
		gsub(" not that far ", " close to ", .) %>%
		gsub(" other employees ", " employees ", .) %>%
		gsub(" phone calls ", " calls ", .) %>%
		
		
		gsub(" cube ", " cubicle ", .) %>%
		
		
		gsub(" having ", " have ", .) %>%
		gsub(" has ", " have ", .) %>%
		
		gsub(" are ", " is ", .) %>%
		gsub(" was ", " is ", .) %>%
		
		
		gsub(" privacy ", " private ", .) %>%
		gsub(" confidential ", " private ", .) %>%
		gsub(" sensitive ", " private ", .) %>%
		
		
		gsub(" noisy ", " loud ", .) %>%
		gsub(" noise ", " loud ", .) %>%
		
		
		
		
		gsub(" speak ", " talk ", .) %>%
		gsub(" conversation ", " talk ", .) %>%
		gsub(" conversations ", " talk ", .) %>%
		gsub(" calls ", " talk ", .) %>%
		gsub(" call ", " talk ", .) %>%
		gsub(" meet ", " talk ", .) %>%
		gsub(" discuss ", " talk ", .) %>%
		
		gsub(" collaboration ", " collaborate ", .) %>%
		gsub(" collaborative ", " collaborate ", .) %>%
		
		gsub(" classroom ", " class ", .) %>%
		gsub(" classrooms ", " class ", .) %>%
		
		gsub(" writing ", " write ", .) %>%
		
		gsub(" grading ", " grade ", .) %>%
		
		gsub(" focus ", " think ", .) %>%
		gsub(" research ", " think ", .) %>%
		gsub(" concentrate ", " think ", .) %>%
		
		gsub(" prep ", " prepare ", .) %>%
		gsub(" preparing ", " prepare ", .) %>%
		gsub(" plan ", " prepare ", .) %>%
		gsub(" planning ", " prepare ", .) %>%
		
		gsub(" near ", " close ", .) %>%
		gsub(" shut ", " close ", .) %>%
		
		gsub(" ability ", " able ", .) %>%
		
		gsub(" accessible ", " access ", .) %>%
		
		gsub(" accommodation ", " accommodate ", .) %>%
		gsub(" accommodations ", " accommodate ", .) %>%
		
		gsub(" advisors ", " adviser ", .) %>%
		gsub(" advising ", " advise ", .)
	
	
	
	
	if(remove_stopwords){
		
		for(i in 1:length(stop_words$word)){
			
			res = gsub(paste0("\\<", stop_words$word[i], "\\>"), "", res)
			
		}
		
		
	}
	
	
	
	if(remove_suffixes){
		#find and remove suffixes
		#all the words
		temp = res %>%
			tibble(text = .) %>%
			unnest_tokens(ngram, text, token = "ngrams", n = 1)
		
		
		suffixes = c("s", "es", "d", "ed", "ing", "ly", "ful", "ies")
		
		for(i in 1:length(suffixes)){
			suf = suffixes[i]
			
			#find all words ending with the suffix that are >3 chars long
			strings = grep(paste0("([[:alpha:]]*?)", suf, "$"), 
										 temp$ngram , value=T) %>%
				unique() %>% 
				.[nchar(.) >3] 
			
			
			#for all the words with the suffix... 
			for(a in seq_along(strings)){
				word = strings[a]
				#see if there is a version of the word without the suffix...
				root = gsub(paste0("([[:alpha:]]*?)", suf, "$"), "\\1", word)
				
				#if so, replace the suffix version of the word with the root word
				if(paste0(root, "e") %in% temp$ngram){
					res = gsub(word, paste0(root, "e"), res)
				} else if(paste0(root, "y") %in% temp$ngram){
					res = gsub(word, paste0(root, "y"), res)
				} else if(root %in% temp$ngram){
					res = gsub(word, root, res)
				}
			}
		}
		
	}
	
	
	if(remove_whitespace){
		
		res = gsub("\\s{2,}", " ", res)
		
	}
	
	res
	
}





bigrams = function(x, preprocess = T, drop_numbers = T,
									 filter = c("hour", "staff", "current", "faculty",
									 					 "helpful", "previous", "employer", 
									 					 "chair")){
	
	if(preprocess & drop_numbers){
		res = 
			x %>%
			preprocess_text() %>%
			.[. != " na "] %>%
			removeNumbers(.) %>%
			tibble(text = .) %>%
			unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
			separate(ngram, c("word1", "word2"), sep = " ") %>%
			filter(!word1 %in% stop_words$word) %>%
			filter(!word2 %in% stop_words$word) %>%
			filter(!word1 %in% filter) %>%
			filter(!word2 %in% filter)
		
	} 
	
	if(preprocess & !drop_numbers){
		res = 
			x %>%
			preprocess_text() %>%
			.[. != " na "] %>%
			tibble(text = .) %>%
			unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
			separate(ngram, c("word1", "word2"), sep = " ") %>%
			filter(!word1 %in% stop_words$word) %>%
			filter(!word2 %in% stop_words$word) %>%
			filter(!word1 %in% filter) %>%
			filter(!word2 %in% filter)
		
	} 
	
	if(!preprocess & drop_numbers){
		res = 
			x %>%
			removeNumbers(.) %>%
			tibble(text = .) %>%
			unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
			separate(ngram, c("word1", "word2"), sep = " ") %>%
			filter(!word1 %in% stop_words$word) %>%
			filter(!word2 %in% stop_words$word) %>%
			filter(!word1 %in% filter) %>%
			filter(!word2 %in% filter)
		
	} 
	
	if(!preprocess & !drop_numbers){
		res = 
			x %>%
			tibble(text = .) %>%
			unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
			separate(ngram, c("word1", "word2"), sep = " ") %>%
			filter(!word1 %in% stop_words$word) %>%
			filter(!word2 %in% stop_words$word) %>%
			filter(!word1 %in% filter) %>%
			filter(!word2 %in% filter)
		
	} 
	
	
	
	res
	
	
}

	
	
	
	

plot_bigrams = function(bigrams, limit = 6, seed=25){
	
	set.seed(seed)
	bigrams %>% 
		count(word1, word2, sort=T) %>%
		filter(n>limit) %>% #need to tune for each
		graph_from_data_frame() %>%
		plot(., margin = -0.1)
	
	
}



	
	
	

word_cloud = function(x, drop_non_word = T, drop_digits = T, 
											max_words = 200, min_freq = 5){
	
	
	temp = x %>% 
		preprocess_text() %>%
		.[. != " na "] %>%
		str_split(., " ") %>% 
		unlist()
	
	if(drop_non_word){
		temp = temp %>%
			gsub(pattern = "\\W", replacement = "", x = .) #get rid of any nonword characters
	}
	
	if(drop_digits){
		temp = temp %>%
			gsub(pattern = "\\d", replacement = "", x = .) #get rid of any digits
	}
	
	temp = temp %>%
		ifelse(. == "", NA_character_, .) %>%
		.[!is.na(.)] %>% 
		plyr::count() %>%
		.[order(.$freq, decreasing = T), ]
	
	
	wordcloud(words = temp$x, freq = temp$freq, min.freq = min_freq,
						max.words=max_words, random.order=FALSE, rot.per=0.35, 
						colors=c("#244061", "#63a038", "#0289b2")
	)
	
}



	
	
	
	
sentiment_contrib = function(x, preprocess = T, filter = c("work")){
	
	
	if(preprocess){
		x = preprocess_text(x)
	}
	
	temp = tibble(text = x)
	temp = unnest_tokens(temp, output = "word", input = "text")
	
	
	
	word_counts = temp %>%
		inner_join(get_sentiments("bing"), by = "word") %>%
		filter(!word %in% filter) %>%
		count(word, sentiment, sort = TRUE)
	
	
	
	
	word_counts %>%
		group_by(sentiment) %>%
		top_n(10) %>%
		ungroup() %>%
		mutate(word = reorder(word, n)) %>%
		ggplot(aes(word, n, fill = sentiment)) +
		geom_col(show.legend = FALSE) +
		facet_wrap(~sentiment, scales = "free_y") +
		labs(y = "Contribution to sentiment",
				 x = NULL) +
		coord_flip()
	
	
	
	
}


	
	
	
	
sentiment_word_cloud = function(x, preprocess = T, filter = c("work")){
	
	if(preprocess){
		x = preprocess_text(x)
	}
	
	temp = tibble(text = x)
	temp = unnest_tokens(temp, output = "word", input = "text")
	
	
	temp2 = temp %>%
		inner_join(get_sentiments("bing"), by = "word") %>%
		count(word, sentiment, sort = TRUE) %>%
		acast(word ~ sentiment, value.var = "n", fill = 0)
	
	colnames(temp2) = c("Negative", "Positive")
	
	temp2 = temp2[!rownames(temp2) %in% filter, ]
	
	
	temp2 %>%
		comparison.cloud(colors = c("firebrick", "blue"),
										 max.words = 100, title.bg.colors = "white")
	
	
	
	cat("Positive to negative ratio: ", sum(temp2[, "Positive"]) / sum(temp2[, "Negative"]))
	
	
	
	
	
}






