/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.text.ParseException;
import java.util.Date;


/**
 * Information about a recent change.
 */
public class RecentChange implements Comparable<RecentChange> {

  // Types of recent changes
  public final static String TYPE_NEW = "new";
  public final static String TYPE_EDIT = "edit";
  public final static String TYPE_LOG = "log";

  // Types of logs
  public final static String LOG_TYPE_BLOCK = "block";
  public final static String LOG_TYPE_DELETE = "delete";
  public final static String LOG_TYPE_MOVE = "move";
  public final static String LOG_TYPE_NEWUSERS = "newusers";
  public final static String LOG_TYPE_PROTECT = "protect";
  public final static String LOG_TYPE_UPLOAD = "upload";

  // Actions for logs
  public final static String LOG_ACTION_BLOCK_BLOCK = "block";
  public final static String LOG_ACTION_BLOCK_REBLOCK = "reblock";
  public final static String LOG_ACTION_BLOCK_UNBLOCK = "unblock";
  public final static String LOG_ACTION_DELETE_DELETE = "delete";
  public final static String LOG_ACTION_DELETE_RESTORE = "restore";
  public final static String LOG_ACTION_DELETE_REVISION = "revision";
  public final static String LOG_ACTION_MOVE_MOVE = "move";
  public final static String LOG_ACTION_MOVE_MOVEREDIR = "move_redir";
  public final static String LOG_ACTION_NEWUSERS_CREATE = "create";
  public final static String LOG_ACTION_PROTECT_PROTECT = "protect";
  public final static String LOG_ACTION_UPLOAD_OVERWRITE = "overwrite";
  public final static String LOG_ACTION_UPLOAD_UPLOAD = "upload";

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
   * Revision identifier.
   */
  private final int revisionId;

  /**
   * Type of change (new, edit, log, ...).
   */
  private String type;

  /**
   * Type of log for logs.
   */
  private String logType;

  /**
   * Type of action for logs.
   */
  private String logAction;

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
   * True if this is a redirect.
   */
  private boolean isRedirect;

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
   * @param revId Revision identifier.
   */
  public RecentChange(
      int id, int namespace,
      String title, int pageId, int revId) {
    this.id = id;
    this.namespace = namespace;
    this.title = title;
    this.pageId = pageId;
    this.revisionId = revId;
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
   * @return Revision identifier.
   */
  public int getRevisionId() {
    return revisionId;
  }

  /**
   * @return Type of change.
   */
  public String getType() {
    return type;
  }

  /**
   * @return Type of log.
   */
  public String getLogType() {
    return logType;
  }

  /**
   * @return Action for the log.
   */
  public String getLogAction() {
    return logAction;
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
   * @return True if this is a redirect.
   */
  public boolean isRedirect() {
    return isRedirect;
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
   * @param type Type of log.
   */
  public void setLogType(String type) {
    this.logType = type;
  }

  /**
   * @param action Action for log.
   */
  public void setLogAction(String action) {
    this.logAction = action;
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
   * @param isBot True if this is a bot edit.
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
   * @param isRedirect True if this is a redirect.
   */
  public void setRedirect(boolean isRedirect) {
    this.isRedirect = isRedirect;
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

  /**
   * @return Hash code.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return id;
  }

  /**
   * @param obj Object to be compared.
   * @return True if objects are equal.
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (obj instanceof RecentChange) {
      return id == ((RecentChange) obj).id;
    }
    return false;
  }

  /**
   * @param o Object to be compared.
   * @return  A negative integer, zero, or a positive integer as this object
   *    is less than, equal to, or greater than the specified object.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(RecentChange o) {
    if (o.id != id) {
      return (o.id < id) ? -1 : 1;
    }
    return 0;
  }
}
