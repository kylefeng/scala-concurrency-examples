import scala.collection._
import scala.concurrent._
import java.util.concurrent.atomic._

package org {
  package object learningconcurrency {
    def log(s: String) = println(s)

    def thread(body: => Unit): Thread = {
      val t = new Thread {
        override def run() = body
      }
      t.start()
      t
    }
    
    private val uid = new AtomicLong(0)
    def getUniqueId(): Long = uid.incrementAndGet()

    class Account(val name: String, var money: Int) {
      val uid = getUniqueId()
    }

    private val transfer = mutable.ArrayBuffer[String]()
    def logTransfer(name: String, n: Int) = transfer.synchronized {
      transfer += s"transfer to account '$name' = $n"
    }

    def add(account: Account, n: Int) = account.synchronized {
      account.money += n
      if (n > 10) logTransfer(account.name, n)
    }

    def send(a: Account, b: Account, n: Int) {
      def adjust() {
        a.money -= n
        b.money += n
      }

      if (a.uid < b.uid) {
        a.synchronized { b.synchronized { adjust() } }
      } else {
        b.synchronized { a.synchronized { adjust() } }
      }
    }

    private val tasks = mutable.Queue[() => Unit]()
    object Worker extends Thread {
      setDaemon(true)

      var terminated = false
      def poll(): Option[() => Unit] = tasks.synchronized {
        while (tasks.isEmpty && !terminated) tasks.wait()
        if (!terminated) Some(tasks.dequeue()) else None
      }

      import scala.annotation.tailrec
      @tailrec override def run() = poll() match {
        case Some(task) => task(); run()
        case None =>
      }

      def shutdown() = tasks.synchronized {
        terminated = true
        tasks.notify()
      }
    }

    // Worker.start()

    def asynchronous(body: => Unit) = tasks.synchronized {
      tasks.enqueue(() => body)
      tasks.notify()
    }

    def execute(body: => Unit) = ExecutionContext.global.execute(
      new Runnable { def run() = body }
    )

    private val lock = new AtomicBoolean(false)
    def mySynchronized(body: => Unit) = {
      while (!lock.compareAndSet(false, true)) {}
      try body finally lock.set(false)
    }
    
  }
}
