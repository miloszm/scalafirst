package com.mimu.scalatest

import org.scalatest.FunSpec
import org.scalatest.Matchers

/**
 * Created by mm.
 * From: Testing in Scala - Daniel Hinojosa
 */

case class Album(name:String)

class AlbumTest extends FunSpec with Matchers {
  describe("An Album") {
    it ("can create album with a name") {
      val album = Album("Dark Side of the Moon")
      album.name should be ("Dark Side of the Moon")
    }
  }

  describe("Dummy") {
    it ("starts with proper regex") {
      val string = """I fell into"""
      string should startWith regex ("I.fel+")
    }
  }
}
