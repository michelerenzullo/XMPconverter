#ifndef LIGHTMD5_H
#define LIGHTMD5_H
#include <string>

std::string md5_process(uint8_t *, size_t);

template <typename T> std::string md5(T initial_msg, size_t initial_len){ 
 if constexpr (std::is_same_v<T,std::string>)
  return md5_process((uint8_t*)initial_msg.c_str(),initial_len);
 else
  return md5_process((uint8_t*)initial_msg,initial_len);
}

#endif