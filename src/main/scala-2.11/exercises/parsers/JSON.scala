package exercises.parsers

/**
 * Created by steve on 9/16/2015.
 */
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String,JSON]) extends JSON

  def JSONParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
    import P._
    val spaces = regex("\\s".r).slice

    def trimLeft[A](p: Parser[A]): Parser[A] = (spaces andThen p) map (x => x._2)
    val QuotedString: Parser[String] = {
      (trimLeft(char('"')) andThen regex("\\S+".r) andThen char('"')) map (x=>x._1._2)
    }

    val JSONNumber: Parser[JNumber] = trimLeft(regex("[0-9]*(\\.[0-9]+)?".r)) map (s => JNumber(s.toDouble))
    val JSONString: Parser[JString] = QuotedString map (s=>JString(s))
    val JSONBool: Parser[JBool] = trimLeft(string("true") or string("false")) map (s => JBool(s=="true"))
    def JSONArrayList: Parser[List[JSON]] = (JSONAnyValue andThen many(trimLeft(char(',')) andThen JSONAnyValue)) map (x=>x._1::(x._2 map (e=>e._2)))
    def JSONArray: Parser[JArray] = (trimLeft(char('[')) andThen JSONArrayList) map (x=>JArray(x._2.toIndexedSeq))
    def JSONObject: Parser[JObject] = {
      (char('{') andThen KeyValueList andThen trimLeft(char('}'))) map (x => x._1._2)
    }
    def JSONAnyValue: Parser[JSON] = JSONNumber or JSONString or JSONBool or JSONObject
    def JSONAny: Parser[JSON] = JSONAnyValue or JSONArray
    def KeyValue: Parser[(String,JSON)] = {
      (QuotedString andThen trimLeft(char(':')) andThen JSONAny) map (x=>(x._1._1,x._2))
    }
    def KeyValueList: Parser[JObject] = {
      def mapFn(l: List[(String,JSON)]) = JObject(l.toMap)
      KeyValue.many map(mapFn)
    }

    trimLeft(JSONObject)
  }
}