/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

import java.text.ParseException;
import java.util.Date;


/**
 * Information about a recent change.
 */
public class RecentChange {

  /**
   * Recent change identifier.
   */
  private final int id;

  /**
   * Namespace.
   */
  private final int namespace;

  /**
   * Page title.
   */
  private final String title;

  /**
   * Page identifier.
   */
  private final int pageId;

  /**
   * Type of change (new, edit, log, ...).
   */
  private String type;

  /**
   * User at the origin of the change.
   */
  private String user;

  /**
   * True if this is a minor edit.
   */
  private boolean isMinor;

  /**
   * True if this edit has been made by an anonymous user.
   */
  private boolean isAnonymous;

  /**
   * True if this is a bot edit.
   */
  private boolean isBot;

  /**
   * True if this is a creation.
   */
  private boolean isNew;

  /**
   * Timestamp of the change.
   */
  private Date timestamp;

  /**
   * Comment.
   */
  private String comment;

  /**
   * @param id Recent change identifier.
   * @param namespace Namespace.
   * @param title Page title.
   * @param pageId Page identifier.
   */
  public RecentChange(int id, int namespace, String title, int pageId) {
    this.id = id;
    this.namespace = namespace;
    this.title = title;
    this.pageId = pageId;
  }

  /**
   * @return Identifier.
   */
  public int getId() {
    return id;
  }

  /**
   * @return Namespace.
   */
  public int getNamespace() {
    return namespace;
  }

  /**
   * @return Page title.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @return Page identifier.
   */
  public int getPageId() {
    return pageId;
  }

  /**
   * @return Type of change.
   */
  public String getType() {
    return type;
  }

  /**
   * @return User at the origin of the change.
   */
  public String getUser() {
    return user;
  }

  /**
   * @return True if this is a minor edit.
   */
  public boolean isMinor() {
    return isMinor;
  }

  /**
   * @return True if this edit has been made by an anonymous user.
   */
  public boolean isAnonymous() {
    return isAnonymous;
  }

  /**
   * @return True if this is a bot edit.
   */
  public boolean isBot() {
    return isBot;
  }

  /**
   * @return True if this is a page creation.
   */
  public boolean isNew() {
    return isNew;
  }

  /**
   * @return Timestamp of the change.
   */
  public Date getTimestamp() {
    return timestamp;
  }

  /**
   * @return Comment.
   */
  public String getComment() {
    return comment;
  }

  /**
   * @param type Type of change.
   */
  public void setType(String type) {
    this.type = type;
  }

  /**
   * @param user User at the origin of the change.
   */
  public void setUser(String user) {
    this.user = user;
  }

  /**
   * @param isMinor True if this is a minor edit.
   */
  public void setMinor(boolean isMinor) {
    this.isMinor = isMinor;
  }

  /**
   * @param isAnonymous True if this edit has been made by an anonymous user.
  */
  public void setAnonymous(boolean isAnonymous) {
    this.isAnonymous = isAnonymous;
  }

  /**
   * @param bot True if this is a bot edit.
   */
  public void setBot(boolean isBot) {
    this.isBot = isBot;
  }

  /**
   * @param isNew True if this is a page creation.
   */
  public void setNew(boolean isNew) {
    this.isNew = isNew;
  }

  /**
   * @param timestamp Timestamp of the change.
   */
  public void setTimestamp(String timestamp) {
    try {
      this.timestamp = DataManager.convertIso8601DateTime(timestamp);
    } catch (ParseException e) {
      this.timestamp = null;
    }
  }

  /**
   * @param comment Comment.
   */
  public void setComment(String comment) {
    this.comment = comment;
  }
}
