package com.mimu.promise


import concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * from neophyte's guide to scala
  */
object PromiseClass extends App {

  case class TaxCut(reduction: Int)

  val taxcut = Promise[TaxCut]()

  val taxcutFuture = taxcut.future

  val p = taxcut.success(TaxCut(14))

  object Government {
    def redeemCampaignPledge(): Future[TaxCut] = {
      val p = Promise[TaxCut]()
      Future {
        println("Starting the new legislative period.")
        Thread.sleep(2000)
        p.success(TaxCut(20))
        println("We reduced the taxes! You must reelect us!!!!1111")
      }
      p.future
    }
  }

  val f = Government.redeemCampaignPledge()

  f.onSuccess{
    case taxcut:TaxCut => println(s"taxcut $taxcut")
  }



}
