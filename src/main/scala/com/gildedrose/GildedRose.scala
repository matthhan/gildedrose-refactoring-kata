package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def shouldJustLoseQuality(item:Item) =
    !item.name.equals("Aged Brie") &&
      !item.name.equals("Backstage passes to a TAFKAL80ETC concert") &&
      !item.name.equals("Sulfuras, Hand of Ragnaros") &&
      item.quality > 0
  def updateQuality() {
    for (i <- 0 until items.length) {
      if (shouldJustLoseQuality(items(i))) {
        val qualityLoss = if(items(i).sellIn <= 0) 2 else 1
        items(i).quality = Math.max(items(i).quality - qualityLoss,0)
      }
      if(items(i).name.equals("Aged Brie")) {
        val qualityIncrease = if(items(i).sellIn <= 0) 2 else 1
        items(i).quality = Math.min(items(i).quality + qualityIncrease, 50)
      }
      if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        val qualityIncrease = if(items(i).sellIn >= 11) 1 else if(items(i).sellIn >= 6) 2 else 3
        items(i).quality = Math.min(items(i).quality + qualityIncrease,50)
        if(items(i).sellIn == 0) items(i).quality = 0
      }

      if (hasToBeSold(items(i))) {
        items(i).sellIn = items(i).sellIn - 1
      }
    }
  }

  private def hasToBeSold(i: Item) = {
    !i.name.equals("Sulfuras, Hand of Ragnaros")
  }
}