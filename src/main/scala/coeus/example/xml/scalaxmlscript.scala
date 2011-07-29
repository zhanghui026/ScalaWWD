package coeus.example.xml

object SampleScript extends App{
  /***此<?xml version="1.0" encoding="ISO-8859-1"?> **/
  val sampleXML =
    <bookstore>
      <book category="COOKING">
        <title lang="en">Everyday Italian</title>
        <author>Giada De Laurentiis</author>
        <year>2005</year>
        <price>30.00</price>
      </book>

      <book category="CHILDREN">
        <title lang="en">Harry Potter</title>
        <author>J K. Rowling</author>
        <year>2005</year>
        <price>29.99</price>
      </book>

      <book category="WEB">
        <title lang="en">XQuery Kick Start</title>
        <author>James McGovern</author>
        <author>Per Bothner</author>
        <author>Kurt Cagle</author>
        <author>James Linn</author>
        <author>Vaidyanathan Nagarajan</author>
        <year>2003</year>
        <price>49.99</price>
      </book>

      <book category="WEB">
        <title lang="en">Learning XML</title>
        <author>Erik T. Ray</author>
        <year>2003</year>
        <price>39.95</price>
      </book>

    </bookstore>

  //select all the titles
    val allTtls = sampleXML \\ "title"
  //select the title of the fist book

  //select all the prices

  //select price nodes with price > 35

  //select title nodes with price > 35
  (sampleXML \ "book") filter (book => (book \ "price").text.toDouble > 35) \\ "title"
}