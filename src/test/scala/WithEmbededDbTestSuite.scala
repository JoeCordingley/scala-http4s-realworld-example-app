package test.io.rw.app

import cats.effect.IO
import com.opentable.db.postgres.embedded.EmbeddedPostgres
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import org.flywaydb.core.Flyway
import utest.*

trait WithEmbededDbTestSuite extends TestSuite {

  // use different port each time since tests run in parallel
  val pg = EmbeddedPostgres.builder().start()
  val port = pg.getPort()
  val fw = Flyway.configure().dataSource(s"jdbc:postgresql://localhost:$port/postgres", "postgres", "postgres").load()

  val xa = Transactor.fromDriverManager[IO]("org.postgresql.Driver", s"jdbc:postgresql://localhost:$port/postgres", "postgres", "postgres")

  override def utestAfterAll(): Unit = {
    pg.close()
  }

  override def utestBeforeEach(path: Seq[String]): Unit = {
    fw.migrate()
  }

  override def utestAfterEach(path: Seq[String]): Unit = {
    fw.clean()
  }
}
