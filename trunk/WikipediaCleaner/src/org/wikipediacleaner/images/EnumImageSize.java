/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.images;


/**
 * Enumeration for possible image sizes.
 */
public enum EnumImageSize {

  SMALL(16, "small"),
  NORMAL(22, "normal"),
  BIG(32, "big"),
  VERY_BIG(64, "verybig");

  private final int size;
  private final String folder;

  /**
   * @param size Image size.
   * @param folder Folder containing the images.
   */
  EnumImageSize(int size, String folder) {
    this.size = size;
    this.folder = folder;
  }

  /**
   * @return Image size.
   */
  public int getSize() {
    return size;
  }

  /**
   * @return Folder containing the images.
   */
  public String getFolder() {
    return folder;
  }
}
