object Program extends App
{
  val minute = 60
  val validityTimeSeconds = minute * 60

  val dataStorage = new DataStorage(validityTimeSeconds)
  val feedMessages = dataStorage.getFeed()

  println(s"Feed news for last " + validityTimeSeconds / minute + " minutes:\n")

  feedMessages.foreach(println)

  print("Enter the key word for indexing: ")

  var keyWord = scala.io.StdIn.readLine()

  while (dataStorage.stopWords.contains(keyWord))
  {
    println("We can't make indexing with this word, please try again")
    print("The key word for indexing: ")

    keyWord = scala.io.StdIn.readLine()
  }

  val indexedFeedMessages: List[(Int, FeedMessage)] = dataStorage.getIndexedFeed(feedMessages, keyWord)

  println("Indexed news:\n")
  for (feed <- indexedFeedMessages)
  {
    println(s"Feed has word count: " + feed._1)
    println(feed._2)
  }

  dataStorage.saveFeed(feedMessages)
}
