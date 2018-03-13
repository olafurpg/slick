package slick.jdbc

import java.sql.{Blob, Clob, Date, Time, Timestamp, ResultSet, PreparedStatement}
import java.util.UUID

import scala.reflect.ClassTag

import slick.SlickException
import slick.ast._
import slick.relational.{RelationalProfile, RelationalTypesComponent}
import java.lang
import scala.`package`.BigDecimal
import slick.ast.ScalaBaseType
import slick.jdbc.JdbcType

trait JdbcTypesComponent extends RelationalTypesComponent { self: JdbcProfile =>

  abstract class MappedJdbcType[T, U](implicit val tmd: JdbcType[U], val classTag: ClassTag[T]) extends JdbcType[T] {
    def map(t: T): U
    def comap(u: U): T

    def newSqlType: Option[Int] = None
    def newSqlTypeName(size: Option[FieldSymbol]): Option[String] = None
    def newValueToSQLLiteral(value: T): Option[String] = None
    def newHasLiteralForm: Option[Boolean] = None

    def sqlType: Int = newSqlType.getOrElse(tmd.sqlType)
    def sqlTypeName(sym: Option[FieldSymbol]): String = newSqlTypeName(sym).getOrElse(tmd.sqlTypeName(sym))
    def setValue(v: T, p: PreparedStatement, idx: Int): Unit = tmd.setValue(map(v), p, idx)
    def setNull(p: PreparedStatement, idx: Int): Unit = tmd.setNull(p, idx)
    def getValue(r: ResultSet, idx: Int): T = {
      val v = tmd.getValue(r, idx)
      if((v.asInstanceOf[AnyRef] eq null) || tmd.wasNull(r, idx)) null.asInstanceOf[T]
      else comap(v)
    }
    def wasNull(r: ResultSet, idx: Int): Boolean = tmd.wasNull(r, idx)
    def updateValue(v: T, r: ResultSet, idx: Int): Unit = tmd.updateValue(map(v), r, idx)
    def valueToSQLLiteral(value: T): String = newValueToSQLLiteral(value).getOrElse(tmd.valueToSQLLiteral(map(value)))
    def hasLiteralForm: Boolean = newHasLiteralForm.getOrElse(tmd.hasLiteralForm)
    def scalaType: ScalaBaseType[T] = ScalaBaseType[T]
    override def toString: String = s"MappedJdbcType[${classTag.runtimeClass.getName} -> $tmd]"
    override def hashCode: Int = tmd.hashCode() + classTag.hashCode()
    override def equals(o: Any): Boolean = o match {
      case o: MappedJdbcType[_, _] => tmd == o.tmd && classTag == o.classTag
      case _ => false
    }
  }

  object MappedJdbcType extends MappedColumnTypeFactory {
    def base[T : ClassTag, U : BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T] = {
      assertNonNullType(implicitly[BaseColumnType[U]])
      new MappedJdbcType[T, U] with BaseTypedType[T] {
        def map(t: T): U = tmap(t)
        def comap(u: U): T = tcomap(u)
      }
    }
  }

  object JdbcType {
    def unapply(t: Type): Some[(JdbcType[Any], Boolean)] = Some((jdbcTypeFor(t), t.isInstanceOf[OptionType]))
  }

  def jdbcTypeFor(t: Type): JdbcType[Any] = ((t.structural match {
    case tmd: JdbcType[_] => tmd
    case ScalaBaseType.booleanType => columnTypes.booleanJdbcType
    case ScalaBaseType.bigDecimalType => columnTypes.bigDecimalJdbcType
    case ScalaBaseType.byteType => columnTypes.byteJdbcType
    case ScalaBaseType.charType => columnTypes.charJdbcType
    case ScalaBaseType.doubleType => columnTypes.doubleJdbcType
    case ScalaBaseType.floatType => columnTypes.floatJdbcType
    case ScalaBaseType.intType => columnTypes.intJdbcType
    case ScalaBaseType.longType => columnTypes.longJdbcType
    case ScalaBaseType.nullType => columnTypes.nullJdbcType
    case ScalaBaseType.shortType => columnTypes.shortJdbcType
    case ScalaBaseType.stringType => columnTypes.stringJdbcType
    case t: OptionType => jdbcTypeFor(t.elementType)
    case t: ErasedScalaBaseType[_, _] => jdbcTypeFor(t.erasure)
    case t => throw new SlickException("JdbcProfile has no JdbcType for type "+t)
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]

  def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR(254)")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.DECIMAL => "DECIMAL(21,2)"
    case t => JdbcTypesComponent.typeNames.getOrElse(t,
      throw new SlickException("No SQL type name found in java.sql.Types for code "+t))
  }

  abstract class DriverJdbcType[@specialized T](implicit val classTag: ClassTag[T]) extends JdbcType[T] {
    def scalaType: ScalaBaseType[T] = ScalaBaseType[T]
    def sqlTypeName(sym: Option[FieldSymbol]): String = self.defaultSqlTypeName(this, sym)
    def valueToSQLLiteral(value: T): String =
      if(hasLiteralForm) value.toString
      else throw new SlickException(sqlTypeName(None) + " does not have a literal representation")
    def hasLiteralForm = true
    def wasNull(r: ResultSet, idx: Int): Boolean = r.wasNull()
    def setNull(p: PreparedStatement, idx: Int): Unit = p.setNull(idx, sqlType)
  }

  class JdbcTypes {
    val booleanJdbcType: JdbcTypes.this.BooleanJdbcType = new BooleanJdbcType
    val blobJdbcType: JdbcTypes.this.BlobJdbcType = new BlobJdbcType
    val byteJdbcType: JdbcTypes.this.ByteJdbcType = new ByteJdbcType
    val byteArrayJdbcType: JdbcTypes.this.ByteArrayJdbcType = new ByteArrayJdbcType
    val charJdbcType: JdbcTypes.this.CharJdbcType = new CharJdbcType
    val clobJdbcType: JdbcTypes.this.ClobJdbcType = new ClobJdbcType
    val dateJdbcType: JdbcTypes.this.DateJdbcType = new DateJdbcType
    val doubleJdbcType: JdbcTypes.this.DoubleJdbcType = new DoubleJdbcType
    val floatJdbcType: JdbcTypes.this.FloatJdbcType = new FloatJdbcType
    val intJdbcType: JdbcTypes.this.IntJdbcType = new IntJdbcType
    val longJdbcType: JdbcTypes.this.LongJdbcType = new LongJdbcType
    val shortJdbcType: JdbcTypes.this.ShortJdbcType = new ShortJdbcType
    val stringJdbcType: JdbcTypes.this.StringJdbcType = new StringJdbcType
    val timeJdbcType: JdbcTypes.this.TimeJdbcType = new TimeJdbcType
    val timestampJdbcType: JdbcTypes.this.TimestampJdbcType = new TimestampJdbcType
    val uuidJdbcType: JdbcTypes.this.UUIDJdbcType = new UUIDJdbcType
    val bigDecimalJdbcType: JdbcTypes.this.BigDecimalJdbcType = new BigDecimalJdbcType
    val nullJdbcType: JdbcTypes.this.NullJdbcType = new NullJdbcType

    class BooleanJdbcType extends DriverJdbcType[Boolean] {
      def sqlType: Int = java.sql.Types.BOOLEAN
      def setValue(v: Boolean, p: PreparedStatement, idx: Int): Unit = p.setBoolean(idx, v)
      def getValue(r: ResultSet, idx: Int): Boolean = r.getBoolean(idx)
      def updateValue(v: Boolean, r: ResultSet, idx: Int): Unit = r.updateBoolean(idx, v)
    }

    class BlobJdbcType extends DriverJdbcType[Blob] {
      def sqlType: Int = java.sql.Types.BLOB
      def setValue(v: Blob, p: PreparedStatement, idx: Int): Unit = p.setBlob(idx, v)
      def getValue(r: ResultSet, idx: Int): Blob = r.getBlob(idx)
      def updateValue(v: Blob, r: ResultSet, idx: Int): Unit = r.updateBlob(idx, v)
      override def hasLiteralForm = false
    }

    class ByteJdbcType extends DriverJdbcType[Byte] with NumericTypedType {
      def sqlType: Int = java.sql.Types.TINYINT
      def setValue(v: Byte, p: PreparedStatement, idx: Int): Unit = p.setByte(idx, v)
      def getValue(r: ResultSet, idx: Int): Byte = r.getByte(idx)
      def updateValue(v: Byte, r: ResultSet, idx: Int): Unit = r.updateByte(idx, v)
    }

    class ByteArrayJdbcType extends DriverJdbcType[Array[Byte]] {
      def sqlType: Int = java.sql.Types.BLOB
      def setValue(v: Array[Byte], p: PreparedStatement, idx: Int): Unit = p.setBytes(idx, v)
      def getValue(r: ResultSet, idx: Int): Array[Byte] = r.getBytes(idx)
      def updateValue(v: Array[Byte], r: ResultSet, idx: Int): Unit = r.updateBytes(idx, v)
      override def hasLiteralForm = false
    }

    class ClobJdbcType extends DriverJdbcType[Clob] {
      def sqlType: Int = java.sql.Types.CLOB
      def setValue(v: Clob, p: PreparedStatement, idx: Int): Unit = p.setClob(idx, v)
      def getValue(r: ResultSet, idx: Int): Clob = r.getClob(idx)
      def updateValue(v: Clob, r: ResultSet, idx: Int): Unit = r.updateClob(idx, v)
      override def hasLiteralForm = false
    }

    class CharJdbcType extends DriverJdbcType[Char] {
      def sqlType: Int = java.sql.Types.CHAR
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
      def setValue(v: Char, p: PreparedStatement, idx: Int): Unit = stringJdbcType.setValue(String.valueOf(v), p, idx)
      def getValue(r: ResultSet, idx: Int): Char = {
        val s = stringJdbcType.getValue(r, idx)
        if(s == null || s.isEmpty) ' ' else s.charAt(0)
      }
      def updateValue(v: Char, r: ResultSet, idx: Int): Unit = stringJdbcType.updateValue(String.valueOf(v), r, idx)
      override def valueToSQLLiteral(v: Char): String = stringJdbcType.valueToSQLLiteral(String.valueOf(v))
    }

    class DateJdbcType extends DriverJdbcType[Date] {
      def sqlType: Int = java.sql.Types.DATE
      def setValue(v: Date, p: PreparedStatement, idx: Int): Unit = p.setDate(idx, v)
      def getValue(r: ResultSet, idx: Int): Date = r.getDate(idx)
      def updateValue(v: Date, r: ResultSet, idx: Int): Unit = r.updateDate(idx, v)
      override def valueToSQLLiteral(value: Date): lang.String = "{d '"+value.toString+"'}"
    }

    class DoubleJdbcType extends DriverJdbcType[Double] with NumericTypedType {
      def sqlType: Int = java.sql.Types.DOUBLE
      def setValue(v: Double, p: PreparedStatement, idx: Int): Unit = p.setDouble(idx, v)
      def getValue(r: ResultSet, idx: Int): Double = r.getDouble(idx)
      def updateValue(v: Double, r: ResultSet, idx: Int): Unit = r.updateDouble(idx, v)
    }

    class FloatJdbcType extends DriverJdbcType[Float] with NumericTypedType {
      def sqlType: Int = java.sql.Types.REAL // see http://docs.oracle.com/javase/1.5.0/docs/guide/jdbc/getstart/mapping.html#1055162
      def setValue(v: Float, p: PreparedStatement, idx: Int): Unit = p.setFloat(idx, v)
      def getValue(r: ResultSet, idx: Int): Float = r.getFloat(idx)
      def updateValue(v: Float, r: ResultSet, idx: Int): Unit = r.updateFloat(idx, v)
    }

    class IntJdbcType extends DriverJdbcType[Int] with NumericTypedType {
      def sqlType: Int = java.sql.Types.INTEGER
      def setValue(v: Int, p: PreparedStatement, idx: Int): Unit = p.setInt(idx, v)
      def getValue(r: ResultSet, idx: Int): Int = r.getInt(idx)
      def updateValue(v: Int, r: ResultSet, idx: Int): Unit = r.updateInt(idx, v)
    }

    class LongJdbcType extends DriverJdbcType[Long] with NumericTypedType {
      def sqlType: Int = java.sql.Types.BIGINT
      def setValue(v: Long, p: PreparedStatement, idx: Int): Unit = p.setLong(idx, v)
      def getValue(r: ResultSet, idx: Int): Long = r.getLong(idx)
      def updateValue(v: Long, r: ResultSet, idx: Int): Unit = r.updateLong(idx, v)
    }

    class ShortJdbcType extends DriverJdbcType[Short] with NumericTypedType {
      def sqlType: Int = java.sql.Types.SMALLINT
      def setValue(v: Short, p: PreparedStatement, idx: Int): Unit = p.setShort(idx, v)
      def getValue(r: ResultSet, idx: Int): Short = r.getShort(idx)
      def updateValue(v: Short, r: ResultSet, idx: Int): Unit = r.updateShort(idx, v)
    }

    class StringJdbcType extends DriverJdbcType[String] {
      def sqlType: Int = java.sql.Types.VARCHAR
      def setValue(v: String, p: PreparedStatement, idx: Int): Unit = p.setString(idx, v)
      def getValue(r: ResultSet, idx: Int): lang.String = r.getString(idx)
      def updateValue(v: String, r: ResultSet, idx: Int): Unit = r.updateString(idx, v)
      override def valueToSQLLiteral(value: String): String = if(value eq null) "NULL" else {
        val sb = new StringBuilder
        sb append '\''
        for(c <- value) c match {
          case '\'' => sb append "''"
          case _ => sb append c
        }
        sb append '\''
        sb.toString
      }
    }

    class TimeJdbcType extends DriverJdbcType[Time] {
      def sqlType: Int = java.sql.Types.TIME
      def setValue(v: Time, p: PreparedStatement, idx: Int): Unit = p.setTime(idx, v)
      def getValue(r: ResultSet, idx: Int): Time = r.getTime(idx)
      def updateValue(v: Time, r: ResultSet, idx: Int): Unit = r.updateTime(idx, v)
      override def valueToSQLLiteral(value: Time): lang.String = "{t '"+value.toString+"'}"
    }

    class TimestampJdbcType extends DriverJdbcType[Timestamp] {
      def sqlType: Int = java.sql.Types.TIMESTAMP
      def setValue(v: Timestamp, p: PreparedStatement, idx: Int): Unit = p.setTimestamp(idx, v)
      def getValue(r: ResultSet, idx: Int): Timestamp = r.getTimestamp(idx)
      def updateValue(v: Timestamp, r: ResultSet, idx: Int): Unit = r.updateTimestamp(idx, v)
      override def valueToSQLLiteral(value: Timestamp): lang.String = "{ts '"+value.toString+"'}"
    }

    class UUIDJdbcType extends DriverJdbcType[UUID] {
      def sqlType: Int = java.sql.Types.OTHER
      def setValue(v: UUID, p: PreparedStatement, idx: Int): Unit = p.setBytes(idx, toBytes(v))
      def getValue(r: ResultSet, idx: Int): UUID = fromBytes(r.getBytes(idx))
      def updateValue(v: UUID, r: ResultSet, idx: Int): Unit = r.updateBytes(idx, toBytes(v))
      override def hasLiteralForm = false
      def toBytes(uuid: UUID): Array[Byte] = if(uuid eq null) null else {
        val msb = uuid.getMostSignificantBits
        val lsb = uuid.getLeastSignificantBits
        val buff = new Array[Byte](16)
        for (i <- 0 until 8) {
          buff(i) = ((msb >> (8 * (7 - i))) & 255).toByte
          buff(8 + i) = ((lsb >> (8 * (7 - i))) & 255).toByte
        }
        buff
      }
      def fromBytes(data: Array[Byte]): UUID = if(data eq null) null else {
        var msb = 0L
        var lsb = 0L
        for (i <- 0 until 8) {
          msb = (msb << 8) | (data(i) & 0xff)
        }
        for (i <- 8 until 16) {
          lsb = (lsb << 8) | (data(i) & 0xff)
        }
        new UUID(msb, lsb)
      }
    }

    class BigDecimalJdbcType extends DriverJdbcType[BigDecimal] with NumericTypedType {
      def sqlType: Int = java.sql.Types.DECIMAL
      def setValue(v: BigDecimal, p: PreparedStatement, idx: Int): Unit = p.setBigDecimal(idx, v.bigDecimal)
      def getValue(r: ResultSet, idx: Int): BigDecimal = {
        val v = r.getBigDecimal(idx)
        if(v eq null) null else BigDecimal(v)
      }
      def updateValue(v: BigDecimal, r: ResultSet, idx: Int): Unit = r.updateBigDecimal(idx, v.bigDecimal)
    }

    class NullJdbcType extends DriverJdbcType[Null] {
      def sqlType: Int = java.sql.Types.NULL
      def setValue(v: Null, p: PreparedStatement, idx: Int): Unit = p.setString(idx, null)
      override def setNull(p: PreparedStatement, idx: Int): Unit = p.setString(idx, null)
      def getValue(r: ResultSet, idx: Int) = null
      def updateValue(v: Null, r: ResultSet, idx: Int): Unit = r.updateNull(idx)
      override def valueToSQLLiteral(value: Null) = "NULL"
    }
  }

  trait ImplicitColumnTypes extends super.ImplicitColumnTypes {
    implicit def booleanColumnType: columnTypes.BooleanJdbcType = columnTypes.booleanJdbcType
    implicit def blobColumnType: columnTypes.BlobJdbcType = columnTypes.blobJdbcType
    implicit def byteColumnType: columnTypes.ByteJdbcType = columnTypes.byteJdbcType
    implicit def byteArrayColumnType: columnTypes.ByteArrayJdbcType = columnTypes.byteArrayJdbcType
    implicit def charColumnType: columnTypes.CharJdbcType = columnTypes.charJdbcType
    implicit def clobColumnType: columnTypes.ClobJdbcType = columnTypes.clobJdbcType
    implicit def dateColumnType: columnTypes.DateJdbcType = columnTypes.dateJdbcType
    implicit def doubleColumnType: columnTypes.DoubleJdbcType = columnTypes.doubleJdbcType
    implicit def floatColumnType: columnTypes.FloatJdbcType = columnTypes.floatJdbcType
    implicit def intColumnType: columnTypes.IntJdbcType = columnTypes.intJdbcType
    implicit def longColumnType: columnTypes.LongJdbcType = columnTypes.longJdbcType
    implicit def shortColumnType: columnTypes.ShortJdbcType = columnTypes.shortJdbcType
    implicit def stringColumnType: columnTypes.StringJdbcType = columnTypes.stringJdbcType
    implicit def timeColumnType: columnTypes.TimeJdbcType = columnTypes.timeJdbcType
    implicit def timestampColumnType: columnTypes.TimestampJdbcType = columnTypes.timestampJdbcType
    implicit def uuidColumnType: columnTypes.UUIDJdbcType = columnTypes.uuidJdbcType
    implicit def bigDecimalColumnType: columnTypes.BigDecimalJdbcType = columnTypes.bigDecimalJdbcType
  }
}

object JdbcTypesComponent {
  private[slick] lazy val typeNames = Map() ++
    (for(f <- classOf[java.sql.Types].getFields)
    yield f.get(null).asInstanceOf[Int] -> f.getName)
}
