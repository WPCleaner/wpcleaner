/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;

import org.wikipediacleaner.api.data.User;


/**
 * Bean for storing connection informations.
 */
public class ConnectionInformation {

  /** Parameter <code>lgToken</code> for API calls */
  private String lgToken;

  /** Parameter <code>lgUserName</code> for API calls */
  private String lgUserName;

  /** Parameter <code>lgUserId</code> for API calls */
  private String lgUserId;

  /** User */
  private User user;

  /** Login token */
  private String loginToken;

  /** Delete token */
  private String deleteToken;

  /** Edit token */
  private String editToken;

  /**
   * Create a clean connection information.
   */
  public ConnectionInformation() {
    clean();
  }

  /**
   * Clean-up connection information.
   */
  public void clean() {
    lgToken = null;
    lgUserName = null;
    lgUserId = null;
    loginToken = null;
    deleteToken = null;
    editToken = null;
  }

  /**
   * @return True if connection is cleaned up.
   */
  public boolean isClean() {
    return (lgToken == null) && (lgUserName == null) && (lgUserId == null);
  }

  /**
   * @param token Parameter <code>lgtoken</code> for API calls.
   * @param userName Parameter <code>lgusername</code> for API calls.
   * @param userId Parameter <code>lguserid</code> for API calls.
   */
  public void setLgInformation(String token, String userName, String userId) {
    setLgToken(token);
    setLgUserName(userName);
    setLgUserId(userId);
  }

  /**
   * @return Parameter <code>lgtoken</code> for API calls.
   */
  public String getLgToken() {
    return lgToken;
  }

  /**
   * @param token Parameter <code>lgtoken</code> for API calls.
   */
  void setLgToken(String token) {
    lgToken = token;
  }

  /**
   * @return Parameter <code>lgusername</code> for API calls.
   */
  public String getLgUserName() {
    return lgUserName;
  }

  /**
   * @param userName Parameter <code>lgUserName</code> for API calls.
   */
  void setLgUserName(String userName) {
    lgUserName = userName;
  }

  /**
   * @return Parameter <code>lguserid</code> for API calls.
   */
  public String getLgUserId() {
    return lgUserId;
  }

  /**
   * @param userId Parameter <code>lgUserId</code> for API calls.
   */
  void setLgUserId(String userId) {
    lgUserId = userId;
  }

  /**
   * @return User.
   */
  public User getUser() {
    return user;
  }

  /**
   * @param user User.
   */
  public void setUser(User user) {
    this.user = user;
  }

  /**
   * @return Login token.
   */
  public String getLoginToken() {
    return loginToken;
  }

  /**
   * @param token Login token.
   */
  public void setLoginToken(String token) {
    this.loginToken = token;
  }

  /**
   * @return Delete token.
   */
  public String getDeleteToken() {
    return deleteToken;
  }

  /**
   * @param token Delete token.
   */
  public void setDeleteToken(String token) {
    this.deleteToken = token;
  }

  /**
   * @return Edit token.
   */
  public String getEditToken() {
    return editToken;
  }

  /**
   * @param token Edit token.
   */
  public void setEditToken(String token) {
    this.editToken = token;
  }
}
