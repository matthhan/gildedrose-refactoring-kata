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
    for (i <- 0 until items.length) {
      determineNewQuality(i)

      if (hasToBeSold(items(i))) {
        items(i).sellIn = items(i).sellIn - 1
      }
    }
  }

  private def determineNewQuality(i: Int) = {
    def getItem = {
      items(i)
    }

    if (shouldJustLoseQuality(getItem)) {
      val qualityLoss = if (getItem.sellIn <= 0) 2 else 1
      getItem.quality = Math.max(getItem.quality - qualityLoss, 0)
    }
    if (isAgedBrie(getItem)) {
      val qualityIncrease = if (getItem.sellIn <= 0) 2 else 1
      getItem.quality = Math.min(getItem.quality + qualityIncrease, 50)
    }
    if (isBackstagePasses(getItem)) {
      val qualityIncrease = if (getItem.sellIn >= 11) 1 else if (getItem.sellIn >= 6) 2 else 3
      getItem.quality = Math.min(getItem.quality + qualityIncrease, 50)
      if (getItem.sellIn == 0) getItem.quality = 0
    }
  }

  private def hasToBeSold(i: Item) = {
    !isSulfuras(i)
  }
}