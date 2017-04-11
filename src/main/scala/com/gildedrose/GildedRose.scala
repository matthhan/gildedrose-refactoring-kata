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

  private def updatedSellDeadline(item: Item) = if (hasToBeSold(item)) item.sellIn - 1 else item.sellIn
  private def hasToBeSold(i: Item) = !isSulfuras(i)

  private def updatedQuality(item: Item):Int = {
      if(isSulfuras(item)) newQualitySulfuras(item)
      else if (isAgedBrie(item)) newQualityAgedBrie(item)
      else if (isBackstagePasses(item)) newQualityBackstagePasses(item)
      else newQualityNormalItem(item)
  }

  private def newQualityBackstagePasses(item: Item) = withinQualityBounds(item.quality + qualityChangeBackstagePasses(item))
  private def newQualityNormalItem(item: Item) = withinQualityBounds(item.quality + (if (item.sellIn <= 0) -2 else -1))
  private def newQualityAgedBrie(item: Item)   = withinQualityBounds(item.quality + (if (item.sellIn <= 0)  2 else  1))
  private def newQualitySulfuras(item:Item)    = item.quality

  private def qualityChangeBackstagePasses(item:Item)=
    if (item.sellIn >= 11) 1
    else if (item.sellIn >= 6) 2
    else if (item.sellIn > 0) 3
    else -item.quality

  private def withinQualityBounds(oldQuality:Int) = withinBounds(0,50)(oldQuality)
  private def withinBounds(lower:Int,upper:Int) = (x:Int) => Math.min(Math.max(x, lower), upper)

}