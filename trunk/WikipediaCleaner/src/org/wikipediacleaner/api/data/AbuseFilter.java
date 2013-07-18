/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.api.data;


/**
 * Bean holding information about abuse filters.
 */
public class AbuseFilter implements Comparable<AbuseFilter> {

  /**
   * Abuse filter identifier.
   */
  private final int id;

  /**
   * Abuse filter description.
   */
  private final String description;

  /**
   * Is abuse filter enabled ?
   */
  private boolean enabled;

  /**
   * Is abuse filter deleted ?
   */
  private boolean deleted;

  /**
   * @param id Abuse filter identifier.
   * @param description Abuse filter description.
   */
  public AbuseFilter(int id, String description) {
    this.id = id;
    this.description = description;
    this.enabled = true;
    this.deleted = false;
  }

  /**
   * @return Abuse filter identifier.
   */
  public int getId() {
    return id;
  }

  /**
   * @return Abuse filter description.
   */
  public String getDescription() {
    return description;
  }

  /**
   * @param enabled True if abuse filter is enabled.
   */
  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * @return True if abuse filter is enabled.
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * @param deleted True if abuse filter is deleted.
   */
  public void setDeleted(boolean deleted) {
    this.deleted = deleted;
  }

  /**
   * @return True if abuse filter is deleted.
   */
  public boolean isDeleted() {
    return deleted;
  }

  /**
   * @return String description.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return Integer.toString(id) + " - " + description;
  }

  /**
   * Compares this abuse filter with the specified abuse filter for order.
   * 
   * @param bl The abuse filter to be compared.
   * @return a negative integer, zero, or a positive integer as this abuse filter
   *         is less than, equal to, or greater the specified abuse filter.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(AbuseFilter bl) {
    return id - bl.id;
  }
}
