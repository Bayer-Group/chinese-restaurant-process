package com.monsanto.stats.tables.clustering

/*
 * The alpha value determines a tradeoff between having more clusters and having clusters that are more
 * homogenous. Alphas is used in the formula that computes the probability of placing an extracted
 * TopicVector into a brand new cluster.
 *
 * The beta is used to add a fake observation for a given vocabulary term---as if we saw that many
 * extra MnMs for every color (because that is what we add to every one). The more data
 * you have, the bigger beta has to be to have any effect. If you don't have a lot of data, or very
 * many categories, 1.0 is a lot for beta.
 *
 * Beta is used for smoothing in the estimateCSmoothingFirst method.
*/
case class ModelParams(topicVectorSize: Int, alpha: Double, beta: Double)
