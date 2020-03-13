package io.rw.app

import doobie._

package object repos {

  // TODO use jdbc logging?
  implicit val logHandler = LogHandler.jdkLogHandler

  def affectedToOption(n: Int): Option[Unit] =
    if (n > 0) Some(()) else None
}
