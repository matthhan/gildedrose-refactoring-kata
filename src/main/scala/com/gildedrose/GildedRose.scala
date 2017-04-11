package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def updateQuality() {
    items.foreach(item => {
      item.quality = updatedQuality(item)
      item.sellIn = updatedSellDeadline(item)
    })
  }

  private def isSulfuras(item: Item) = item.name == "Sulfuras, Hand of Ragnaros"
  private def isBackstagePasses(item: Item) =  item.name == "Backstage passes to a TAFKAL80ETC concert"
  private def isAgedBrie(item: Item) = item.name == "Aged Brie"

  def shouldJustLoseQuality(item:Item) =
    !isAgedBrie(item) &&
      !isBackstagePasses(item) &&
      !isSulfuras(item)

  private def updatedSellDeadline(item: Item) = if (hasToBeSold(item)) item.sellIn - 1 else item.sellIn

  private def updatedQuality(item: Item):Int = {
    val qualityIncrease =
      if (shouldJustLoseQuality(item)) -(if (item.sellIn <= 0) 2 else 1)
      else if (isAgedBrie(item))
        if (item.sellIn <= 0) 2 else 1
      else if (isBackstagePasses(item))
        if (item.sellIn >= 11) 1 else if (item.sellIn >= 6) 2 else 3
      else 0
    val newQuality = if(isBackstagePasses(item) && item.sellIn == 0) 0 else newQualityWithinTheBounds(item,qualityIncrease)
    return newQuality
  }

  private def newQualityWithinTheBounds(item: Item, qualityIncrease: Int) =
    if (qualityIncrease != 0) Math.min(Math.max(item.quality + qualityIncrease, 0), 50) else item.quality

  private def hasToBeSold(i: Item) = !isSulfuras(i)
}