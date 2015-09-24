package exercises.option

/**
 * Created by steve on 9/6/2015.
 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatmap[B](f: A => Option[B]) : Option[B]
  def getOrElse[B >: A](default: => B) : B
  def orElse[B >: A](default: => Option[B]) : Option[B]
  def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing]
{
  override def map[B](f: Nothing => B): Option[B] = None

  override def filter(f: Nothing => Boolean): Option[Nothing] = None

  override def flatmap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B](default: => B): B = default

  override def orElse[B](default: => Option[B]): Option[B] = default
}

case class Some[A](value: A) extends Option[A]
{
  override def map[B](f: (A) => B): Option[B] = Some(f(value))

  override def filter(f: (A) => Boolean): Option[A] = if (f(value)) Some(value) else None

  override def flatmap[B](f: (A) => Option[B]): Option[B] = f(value)

  override def getOrElse[B >: A](default: => B): B = value

  override def orElse[B >: A](default: => Option[B]): Option[B] = this
}

object Option {
  def apply[A](a: A) = new Some(a)
}