package com.gildedrose

import org.scalatest._

class GildedRoseTest extends WordSpec with Matchers {

  "Gilded Roses" when {

    "common item is updated" should {
      val commonItem = new Item(name = "foo", sellIn = 2, quality = 2)
      val app = createGildedRose(commonItem)

      "reduce sellIn and quality by 1" in {
        app.updateQuality()

        app.items.head.sellIn shouldBe 1
        app.items.head.quality shouldBe 1
      }

      "not reduce quality below 0" in {
        app.updateQuality()
        app.updateQuality()
        app.updateQuality()

        app.items.head.quality shouldBe 0
      }
    }

    "Aged Brie is updated" should {
      def agedBrie = new Item(name = "Aged Brie", sellIn = 1, quality = 2)

      "increment quality by one on the first update" in {
        val app = createGildedRose(agedBrie)

        app.updateQuality()

        app.items(0).quality shouldBe 3
      }

      "increment quality by two on subsequent updates" in {
        val app = createGildedRose(agedBrie)

        app.updateQuality()
        app.updateQuality()

        app.items(0).quality shouldBe 5
      }

      "not increment quality above 50" in {
        val app = createGildedRose(agedBrie)

        (1 to 100).foreach { _ =>
          app.updateQuality()
        }

        app.items(0).quality shouldBe 50
      }
    }

    "Sulfuras is updated" should {
      val sulfuras = new Item("Sulfuras, Hand of Ragnaros", 5, 80)
      val app = createGildedRose(sulfuras)

      "not change quality" in {
        app.updateQuality()
        app.updateQuality()

        app.items(0).sellIn shouldBe 5
        app.items(0).quality shouldBe 80
      }
    }

    "Backstage pass is updated" should {

      "increase quality by two when sellIn in (5, 10]" in {
        val pass = new Item("Backstage passes to a TAFKAL80ETC concert", 7, 1)
        val app = createGildedRose(pass)

        app.updateQuality()
        app.updateQuality()

        app.items(0).quality shouldBe 5
      }

      "increase quality by 3 when sellIn is < 5" in {
        val pass = new Item("Backstage passes to a TAFKAL80ETC concert", 4, 1)
        val app = createGildedRose(pass)

        app.updateQuality()
        app.updateQuality()

        app.items(0).quality shouldBe 7
      }

      "set quality to 0 when sellIn is 0" in {
        val pass = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 5)
        val app = createGildedRose(pass)

        app.updateQuality()

        app.items(0).quality shouldBe 0
      }
    }
  }

  private def createGildedRose(items: Item*) = {
    new GildedRose(items.toArray)
  }

}
