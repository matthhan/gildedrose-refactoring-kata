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

      item.sellIn = determineNewSellDeadline(item)
    })
  }

  private def determineNewSellDeadline(item: Item) = {
    if (hasToBeSold(item)) {
      item.sellIn - 1
    } else item.sellIn
  }

  private def determineNewQuality(item: Item) = {
    var qualityIncrease
    if (shouldJustLoseQuality(item)) {
      qualityIncrease = -(if (item.sellIn <= 0) 2 else 1)
    }
    if (isAgedBrie(item)) {
      qualityIncrease = if (item.sellIn <= 0) 2 else 1
    }
    if (isBackstagePasses(item)) {
      qualityIncrease = if (item.sellIn >= 11) 1 else if (item.sellIn >= 6) 2 else 3
    }
    item.quality = Math.min(Math.max(item.quality + qualityIncrease, 0),50)
    if(isBackstagePasses(item) && item.sellIn == 0) item.quality = 0
  }

  private def hasToBeSold(i: Item) = {
    !isSulfuras(i)
  }
}