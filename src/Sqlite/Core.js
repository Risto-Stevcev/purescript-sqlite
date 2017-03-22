"use strict";

// module Sqlite.Core

var sqlite3 = require('sqlite3');

var sqlParamsToObj = function(params) {
  return params.reduce(function(obj, param) {
    obj[param.value0] = param.value1.value0;
    return obj;
  }, {});
}

exports._setVerbose = function() {
  sqlite3.verbose()
}

exports._OPEN_READONLY = sqlite3.OPEN_READONLY;
exports._OPEN_READWRITE = sqlite3.OPEN_READWRITE;
exports._OPEN_CREATE = sqlite3.OPEN_CREATE;

exports._connect = function(filename, mode, cached) {
  return function(success, error) {
    var Database = cached ? sqlite3.cached.Database : sqlite3.Database;

    var db = new Database(filename, mode, function(err) {
      if (err) {
        error(err);
      }
      else {
        success(db);
      }
    });
  }
}

exports._close = function(db) {
  return function(success, error) {
    db.close(function(err) {
      if (err) {
        error(err);
      }
      else {
        success();
      }
    });
  }
}

exports._run = function(db, query) {
  return function(success, error) {
    db.run(query, function(err) {
      if (err) {
        error(err);
      }
      else {
        var lastID = this.lastID; // Only for INSERT
        var changes = this.changes; // Only for UPDATE or DELETE
        success({ lastID: lastID, changes: changes });
      }
    });
  }
}

exports._getOne = function(db, query) {
  return function(success, error) {
    db.get(query, function(err, row) {
      if (err) {
        error(err);
      }
      else {
        success(row === undefined ? null : row);
      }
    });
  }
}

exports._get = function(db, query) {
  return function(success, error) {
    db.all(query, function(err, rows) {
      if (err) {
        error(err);
      }
      else {
        success(rows);
      }
    });
  }
}


exports._stmtPrepare = function(db, query) {
  return function(success, error) {
    var statement = db.prepare(query, function(err) {
      if (err) {
        error(err);
      }
      else {
        success(statement);
      }
    });
  }
}

exports._stmtBind = function(stmt, params) {
  return function(success, error) {
    stmt.bind(sqlParamsToObj(params), function(err) {
      if (err) {
        error(err);
      }
      else {
        success();
      }
    });
  }
}

exports._stmtReset = function(stmt) {
  return function(success, error) {
    stmt.reset(success);
  }
}

exports._stmtFinalize = function(stmt) {
  return function(success, error) {
    stmt.finalize(success);
  }
}

exports._stmtRun = function(stmt, params) {
  return function(success, error) {
    stmt.run(sqlParamsToObj(params), function(err) {
      if (err) {
        error(err);
      }
      else {
        var lastID = this.lastID; // Only for INSERT
        var changes = this.changes; // Only for UPDATE or DELETE
        success({ lastID: lastID, changes: changes });
      }
    });
  }
}

exports._stmtGetOne = function(stmt, params) {
  return function(success, error) {
    stmt.get(sqlParamsToObj(params), function(err, row) {
      if (err) {
        error(err);
      }
      else {
        success(row === undefined ? null : row);
      }
    });
  }
}

exports._stmtGet = function(stmt, params) {
  return function(success, error) {
    stmt.all(sqlParamsToObj(params), function(err, rows) {
      if (err) {
        error(err);
      }
      else {
        success(rows);
      }
    });
  }
}


exports._dbListener = function(fn) {
  return function(result) {
    return fn(result)();
  };
};

exports._dbListenerFn2 = function(fn) {
  return function(result, result2) {
    return fn(result, result2)();
  };
};

exports._listen = function(db, eventType, callback) {
  return function() {
    db.on(eventType, callback);
    return {};
  }
}
