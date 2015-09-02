CREATE TABLE data_type(
    id INTEGER NOT NULL AUTO_INCREMENT,
    name VARCHAR(32),
    PRIMARY KEY(id),
    UNIQUE(name)
);
CREATE TABLE data_type_matcher(
    id INTEGER NOT NULL AUTO_INCREMENT,
    data_type_id INTEGER NOT NULL,
    regex VARCHAR(64) NOT NULL,
    group_key VARCHAR(32),
    item_key VARCHAR(32),
    PRIMARY KEY(id),
    FOREIGN KEY(data_type_id) REFERENCES data_type(id)
);
CREATE TABLE raw_data(
    id INTEGER NOT NULL AUTO_INCREMENT,
    data_type_id INTEGER NOT NULL,
    group_key VARCHAR(32),
    group_value VARCHAR(64),
    item_key VARCHAR(32),
    item_value VARCHAR(256),
    PRIMARY KEY(id),
    FOREIGN KEY(data_type_id) REFERENCES data_type(id),
    INDEX(group_key, group_value)
);
