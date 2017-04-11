package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def shouldJustLoseQuality(item:Item) =
    !isAgedBrie(item) &&
      !isBackstagePasses(item) &&
      !isSulfuras(item) &&
      item.quality > 0

  private def isSulfuras(item: Item) = item.name == "Sulfuras, Hand of Ragnaros"
  private def isBackstagePasses(item: Item) =  item.name == "Backstage passes to a TAFKAL80ETC concert"
  private def isAgedBrie(item: Item) = item.name == "Aged Brie"

  def updateQuality() {
    items.foreach(item => {
      determineNewQuality(item)

      val newSellInDate  = if (hasToBeSold(item)) {
        item.sellIn - 1
      } else item.sellIn
      item.sellIn = newSellInDate
    })
  }

  private def determineNewQuality(item: Item) = {

    if (shouldJustLoseQuality(item)) {
      val qualityLoss = if (item.sellIn <= 0) 2 else 1
      item.quality = Math.max(item.quality - qualityLoss, 0)
    }
    if (isAgedBrie(item)) {
      val qualityIncrease = if (item.sellIn <= 0) 2 else 1
      item.quality = Math.min(item.quality + qualityIncrease, 50)
    }
    if (isBackstagePasses(item)) {
      val qualityIncrease = if (item.sellIn >= 11) 1 else if (item.sellIn >= 6) 2 else 3
      item.quality = Math.min(item.quality + qualityIncrease, 50)
      if (item.sellIn == 0) item.quality = 0
    }
  }

  private def hasToBeSold(i: Item) = {
    !isSulfuras(i)
  }
}