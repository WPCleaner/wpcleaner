/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.images;


/**
 * Enumeration for possible image sizes.
 */
public enum EnumImageSize {

  SMALL(16, "small"),
  NORMAL(22, "normal");

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
