import java.nio.file.{Files, Paths}

import scala.collection.mutable.{HashSet, ListBuffer}
import scala.io.Source
import scala.xml.{Elem, NodeSeq, XML}

class DataStorage( val validityTimeSeconds   : Int    = 60 * 60 * 24, // 1 day
                   val dataStorageFileName   : String = "./resources/dataStorage.xml" ,
                   val dataResourcesFileName : String = "./resources/rssResources.txt",
                   val stopWordsFileName     : String = "./resources/stop-words.txt"
                 )
{
  private lazy val providers : List[String] = Source.fromFile(dataResourcesFileName).getLines.toList

  lazy val stopWords : List[String] = Source.fromFile(stopWordsFileName).getLines.toList


  private def toXml(feedMessages: HashSet[FeedMessage]): Elem =
  {
    <FeedMessages>
      {
        for(feedMessage <- feedMessages) yield feedMessage.toXml
      }
    </FeedMessages>
  }

  private def loadStoredFeed() : HashSet[FeedMessage]=
  {
    val path = Paths.get(dataStorageFileName)

    if(Files.exists(path))
    {
      getFeedFromXmlNodes(XML.loadFile(dataStorageFileName) \ "FeedMessage")
    }

    HashSet.empty
  }

  private def getFeedFromXmlNodes(feedMessagesNodes : NodeSeq) : HashSet[FeedMessage]=
  {
    val feedMessages = new HashSet[FeedMessage]

    for( feedMessageNode <- feedMessagesNodes ){
      val feedMessage = FeedMessage.fromXML(feedMessageNode)

      if (feedMessage.isActual(validityTimeSeconds))
      {
        feedMessages.add(feedMessage)
      }
    }

    feedMessages
  }

  private def loadNewFeed()=
  {
    val loadedRSS = loadRSSMessages(dataResourcesFileName)
    val feedMessages = new HashSet[FeedMessage]

    for( feedMessage <- loadedRSS if feedMessage.isActual(validityTimeSeconds)){
      feedMessages.add(feedMessage)
    }

    feedMessages
  }

  private def loadRSSMessages(sourceFileName : String) : List[FeedMessage] =
  {
    val buf = ListBuffer.empty[FeedMessage]

    for (provider <- providers)
    {
      println(provider)
      val items = XML.load(provider) \ "channel" \\ "item"

      for(item <- items){
        buf.append(FeedMessage.fromXML(item))
      }
    }

    buf.toList
  }

  def saveFeed(feedMessages: HashSet[FeedMessage]) =
  {
    val path = Paths.get(dataStorageFileName)

    Files.deleteIfExists(path)
    Files.createFile(path)

    XML.save(dataStorageFileName, toXml(feedMessages))
  }

  def getFeed(): HashSet[FeedMessage] =
  {
    val feed = loadNewFeed()

    feed.par ++ loadStoredFeed()

    return feed
    return loadStoredFeed()
  }

  def getIndexedFeed(feedMessages: HashSet[FeedMessage], keyWord : String, feedCount : Int = 10): List[(Int, FeedMessage)] =
  {
    def wordCount(str : String): Int =
    {
      str.split("\\W+").count(word => word == keyWord)
    }

    feedMessages
      .map(fm => (wordCount(fm.description) + wordCount(fm.title) + wordCount(fm.link), fm))
      .toList
      .filter(pair => pair._1 > 0)
      .sortWith(_._1 > _._1)
      .take(feedCount)
  }
}