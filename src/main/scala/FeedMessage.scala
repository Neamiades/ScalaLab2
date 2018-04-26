import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.util.concurrent.TimeUnit

import scala.xml.Node

class FeedMessage( val title      : String,
                   val link       : String,
                   val description: String,
                   val publishDate: Date)
{
  def isActual(validityTimeSeconds: Int)=
  {
    val now = new Date()
    val differenceInSeconds = TimeUnit.MILLISECONDS.toSeconds(now.getTime() - publishDate.getTime());
    differenceInSeconds <= validityTimeSeconds
  }

  def canEqual(a: Any) = a.isInstanceOf[FeedMessage]

  override def equals(that: Any): Boolean = that match
  {
      case that: FeedMessage => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
  }

  override def hashCode: Int =
  {
    val prime = 31

    prime + (if (link == null) 0 else link.hashCode)
  }

  def toXml =
  {
    <FeedMessage>
      <title>{title}</title>
      <link>{link}</link>
      <description>{description}</description>
      <pubDate>{FeedMessage.dateFormat.format(publishDate)}</pubDate>
    </FeedMessage>
  }

  override def toString =
    s"[\n\ttitle: $title,\n\tlink: $link,\n\tdescription: $description,\n\tpublishDate: $publishDate\n]"
}

object FeedMessage
{
  val dateFormat    = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US)
  val sqlDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

  def fromXML(item : Node) : FeedMessage =
  {
    def tryGetPubDate(item : Node) : Date = {
      var pubDate = (item \ "pubDate").text

      if (pubDate.isEmpty) {
        pubDate = (item \ "link" \ "pubDate" ).text
      }

      dateFormat.parse(pubDate)
    }

    val title = (item \ "title"      ).text
    val link  = (item \ "link"       ).text
    val desc  = (item \ "description").text

    var date = new Date()

    try {
      date = tryGetPubDate(item)
    }
    catch {
      case e => e.printStackTrace
    }

    new FeedMessage(title, link, desc, date)
  }
}